package dev.cheleb.scalamigen

trait ConditionalFor[C, A]:
    def check: C => Boolean

object ConditionalFor:
    def apply[C, A](f: C => Boolean) = new ConditionalFor[C, A]:
        def check: C => Boolean = f