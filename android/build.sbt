androidBuild
useSupportVectors

// instrumentTestRunner :=
//   "android.support.test.runner.AndroidJUnitRunner"

platformTarget := "android-23"

javacOptions ++= Seq(
  "-source", "1.7",
  "-target", "1.7",
  "-Xlint:unchecked",
  "-Xlint:deprecation"
)

scalacOptions ++= Seq(
  "-target:jvm-1.7",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xcheckinit",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  aar("com.android.support" % "appcompat-v7" % "24.0.0"),
  "com.github.xuwei-k" %% "httpz-native" % "0.4.0",
  "org.scalaz" %% "scalaz-core" % "7.2.4",
  "io.argonaut" %% "argonaut" % "6.1a"
)

proguardOptions += "-keep class scala.Function1"

proguardOptions += "-keep class scala.PartialFunction"

proguardOptions += "-dontwarn org.http4s.**"

proguardOptions += "-dontwarn org.apache.**"

proguardOptions += "-dontwarn javassist.util.**"

proguardOptions += "-dontwarn javassist.tools.**"

proguardOptions += "-dontwarn org.slf4j.**"

proguardOptions += "-dontwarn io.netty.**"

proguardOptions += "-dontwarn scalaz.**"

proguardOptions += "-dontwarn macrocompat.**"

// Shut up the APK packager
packagingOptions in Android := PackagingOptions(
  excludes = Seq(
    "META-INF/NOTICE.txt",
    "META-INF/NOTICE",
    "META-INF/DEPENDENCIES",
    "META-INF/LICENSE.txt",
    "META-INF/LICENSE"
  ))

// Shut up the dex process from running out of heap space
proguardCache += "scalaz"
//dexMaxHeap = "2g"
