# theinventory/android

This is an Android client to theinventory, written in Scala.

The application is purely functional at the `Activity` level, meaning that each
`Activity` is considered a "program" of sorts, and the `onCreate` method is what
is considered the "end of the world" and calls `Task#unsafePerformSync` in every
case.

In `client` is a purely functional client to TheInventory, making use of the
awesome "httpz" library. We use "httpz-native" because I could get it to work on
Android. If we wanted, we could try to get "httpz-apache" to work.

## Setting up

* Clone the repository
* The sbt-android plugin will install an Android SDK for you, or you can do so
  manually and set `ANDROID_HOME` to wherever it lives.
* Use `android avd` to set up an emulator, if you want.
* Change the (for now, hardcoded) URL in
  `src/main/scala/org/noexc/theinventory/client/Command.scala` to point to your
  IP and yell at Ricky to make this a setting in the application.
* Run `sbt compile android:install` (or alternatively, run `sbt` and run
  `compile` and then `android:install` inside of it, rather than having to
  await the JVM each time you want to re-install).
* Start a development server (see the [server docs](../server/README.md) for
  that).
* Make sure your firewall allows connections on whatever port you run the server
  on (default: 8081).
* Run the app.
* Jump for joy.
* Write code.
* Go back to the `sbt` step and do it all over again.
* Notice that this list is infinitely recursive.
* In fact if you follow it in order, you will never read this step.
* Nor this one.
* So I can say whatever I want here and nobody will ever read it. :)

## LICENSE

The client is available under a 3-clause BSD license, the same as the server
component of theinventory.
