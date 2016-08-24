package org.noexc.theinventory

import android.app.Activity
import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.graphics.drawable.Animatable
import android.widget.TextView
import httpz.native._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task


import client._

class MainActivity extends AppCompatActivity {
  implicit val context = this

  def getTags: Task[httpz.Error \/ IList[Tag]] = GetTags().action.task

  def setDemoText(vh: TypedViewHolder.main, s: String): Task[Unit] = for {
    _ <- Task(vh.text.setText(s))
  } yield ()

  def main(savedInstanceState: Bundle): Task[Unit] =
    for {
      _ <- Task(super.onCreate(savedInstanceState))
      vh <- Task(TypedViewHolder.setContentView(this, TR.layout.main))
      tags <- getTags
      _ <- setDemoText(vh, tags.toString)
    } yield ()


  // This is considered the "end of the world"
  // We call run our otherwise purely functional code above here.
  override def onCreate(savedInstanceState: Bundle): Unit =
    main(savedInstanceState).unsafePerformSync
}
