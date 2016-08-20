androidBuild
useSupportVectors

// instrumentTestRunner :=
//   "android.support.test.runner.AndroidJUnitRunner"

platformTarget := "android-23"

javacOptions in Compile ++= "-source" :: "1.7" :: "-target" :: "1.7" :: Nil

libraryDependencies ++=
  aar("com.android.support" % "appcompat-v7" % "24.0.0") ::
  // aar("com.android.support.test" % "runner" % "0.5" % "androidTest") ::
  // aar("com.android.support.test.espresso" % "espresso-core" % "2.2.2" % "androidTest") ::
  Nil
