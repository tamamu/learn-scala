package errorhandling

/**
  * Created by tamamu on 7/6/16.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(get) => Some(f(get))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(get) => get
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse None

  def filter(f: A => Boolean): Option[A] =
    flatMap(get => if (f(get)) Some(get) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]