package io

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/** Класс типов, позволяющий комбинировать описания вычислений, которые могут либо успешно
  * завершиться с некоторым значением, либо завершиться неуспешно, выбросив исключение Throwable.
  *
  * @tparam F
  *   тип вычисления
  */

trait Computation[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]

  def pure[A](a: A): F[A]

  def *>[A, B](fa: F[A])(another: F[B]): F[B]

  def as[A, B](fa: F[A])(newValue: => B): F[B]

  def void[A](fa: F[A]): F[Unit]

  def attempt[A](fa: F[A]): F[Either[Throwable, A]]

  def option[A](fa: F[A]): F[Option[A]]

  /** Если вычисление fa выбрасывает ошибку, то обрабатывает ее функцией f, без изменения типа
    * выходного значения.
    *
    * @return
    *   результат вычисления fa или результат функции f
    */
  def handleErrorWith[A, AA >: A](fa: F[A])(f: Throwable => F[AA]): F[AA]

  /** Обрабатывает ошибку вычисления чистой функцией recover или преобразует результат вычисления
    * чистой функцией.
    *
    * @return
    *   результат вычисления преобразованный функцией map или результат функции recover
    */
  def redeem[A, B](fa: F[A])(recover: Throwable => B, map: A => B): F[B]

  def redeemWith[A, B](fa: F[A])(recover: Throwable => F[B], bind: A => F[B]): F[B]

  /** Выполняет вычисление. "unsafe", потому что при неуспешном завершении может выбросить
    * исключение.
    *
    * @param fa
    *   еще не начавшееся вычисление
    * @tparam A
    *   тип результата вычисления
    * @return
    *   результат вычисления, если оно завершится успешно.
    */
  def unsafeRunSync[A](fa: F[A]): A

  /** Оборачивает ошибку в контекст вычисления.
    *
    * @param error
    *   ошибка
    * @tparam A
    *   тип результата вычисления. Т.к. вычисление сразу завершится ошибкой при выполнении, то может
    *   быть любым.
    * @return
    *   создает описание вычисления, которое сразу же завершается с поданной ошибкой.
    */
  def raiseError[A](error: Throwable): F[A]

}

object Computation {
  def apply[F[_]: Computation]: Computation[F] = implicitly[Computation[F]]
}

final class MyIO[A](private val run: () => A) {
  self =>

  def map[B](f: A => B)(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.map(self)(f)

  def flatMap[B](f: A => MyIO[B])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.flatMap(self)(f)

  def tailRecM[B](f: A => MyIO[Either[A, B]])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.tailRecM(run())(f)

  def *>[B](another: MyIO[B])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.*>(self)(another)

  def as[B](newValue: => B)(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.as(self)(newValue)

  def void(implicit
    comp: Computation[MyIO]
  ): MyIO[Unit] = comp.void(self)

  def attempt(implicit
    comp: Computation[MyIO]
  ): MyIO[Either[Throwable, A]] = comp.attempt(self)

  def option(implicit
    comp: Computation[MyIO]
  ): MyIO[Option[A]] = comp.option(self)

  def handleErrorWith[AA >: A](f: Throwable => MyIO[AA])(implicit
    comp: Computation[MyIO]
  ): MyIO[AA] = comp.handleErrorWith[A, AA](self)(f)

  def redeem[B](recover: Throwable => B, map: A => B)(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.redeem(self)(recover, map)

  def redeemWith[B](recover: Throwable => MyIO[B], bind: A => MyIO[B])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.redeemWith(self)(recover, bind)

  def unsafeRunSync(implicit
    comp: Computation[MyIO]
  ): A = comp.unsafeRunSync(self)

}

object MyIO {

  implicit val computationInstanceForIO: Computation[MyIO] = new Computation[MyIO] {
    override def map[A, B](fa: MyIO[A])(f: A => B): MyIO[B] = flatMap(fa)(x => MyIO(f(x)))

    override def flatMap[A, B](fa: MyIO[A])(f: A => MyIO[B]): MyIO[B] = attempt(fa).run() match {
      case Left(error)  => raiseError(error)
      case Right(value) => f(value)
    }

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => MyIO[Either[A, B]]): MyIO[B] = f(a).run() match {
      case Left(value)  => tailRecM(value)(f)
      case Right(value) => MyIO(value)
    }

    override def pure[A](a: A): MyIO[A] = MyIO(a)

    override def *>[A, B](fa: MyIO[A])(another: MyIO[B]): MyIO[B] = flatMap(fa)(_ => another)

    override def as[A, B](fa: MyIO[A])(newValue: => B): MyIO[B] = map(fa)(_ => newValue)

    override def void[A](fa: MyIO[A]): MyIO[Unit] = flatMap(fa)(_ => pure(()))

    override def attempt[A](fa: MyIO[A]): MyIO[Either[Throwable, A]] = MyIO(
      try
        Right(fa.run())
      catch {
        case error: Throwable => Left(error)
      }
    )

    override def option[A](fa: MyIO[A]): MyIO[Option[A]] = MyIO(attempt(fa).run() match {
      case Left(_)      => None
      case Right(value) => Some(value)
    })

    override def handleErrorWith[A, AA >: A](fa: MyIO[A])(f: Throwable => MyIO[AA]): MyIO[AA] =
      attempt(fa).run() match {
        case Left(error)  => f(error)
        case Right(value) => MyIO[AA](value)
      }

    override def redeem[A, B](fa: MyIO[A])(recover: Throwable => B, map: A => B): MyIO[B] = MyIO(
      attempt(fa).run() match {
        case Left(error)  => recover(error)
        case Right(value) => map(value)
      }
    )

    override def redeemWith[A, B](
      fa: MyIO[A]
    )(recover: Throwable => MyIO[B], bind: A => MyIO[B]): MyIO[B] = flatMap(
      MyIO(
        attempt(fa).run() match {
          case Left(error)  => recover(error)
          case Right(value) => bind(value)
        }
      )
    )(identity)

    override def unsafeRunSync[A](fa: MyIO[A]): A = fa.run()

    override def raiseError[A](error: Throwable): MyIO[A] = delay(throw error)
  }

  def apply[A](body: => A): MyIO[A] = delay(body)

  def suspend[A](thunk: => MyIO[A])(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = comp.flatMap(thunk)(x => MyIO(x))

  def delay[A](body: => A): MyIO[A] = new MyIO[A](() => body)

  def pure[A](a: A): MyIO[A] = MyIO(a)

  def fromEither[A](e: Either[Throwable, A])(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = e match {
    case Left(error)  => MyIO.raiseError(error)(comp)
    case Right(value) => MyIO(value)
  }

  def fromOption[A](option: Option[A])(orElse: => Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = option match {
    case Some(value) => MyIO(value)
    case None        => MyIO.raiseError(orElse)(comp)
  }

  def fromTry[A](t: Try[A])(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = t match {
    case Failure(exception) => MyIO.raiseError(exception)(comp)
    case Success(value)     => MyIO(value)
  }

  def none[A]: MyIO[Option[A]] = MyIO(None)

  def raiseUnless(cond: Boolean)(e: => Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[Unit] = raiseWhen(!cond)(e)(comp)

  def raiseWhen(cond: Boolean)(e: => Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[Unit] = if (cond) MyIO.raiseError(e)(comp) else unit

  def raiseError[A](error: Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = comp.raiseError(error)

  def unlessA(cond: Boolean)(action: => MyIO[Unit]): MyIO[Unit] = if (!cond) action else MyIO(())

  def whenA(cond: Boolean)(action: => MyIO[Unit]): MyIO[Unit] = if (cond) action else MyIO(())

  val unit: MyIO[Unit] = MyIO(())

}
