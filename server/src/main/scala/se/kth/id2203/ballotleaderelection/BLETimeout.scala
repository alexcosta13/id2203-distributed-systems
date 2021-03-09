package se.kth.id2203.ballotleaderelection

import se.sics.kompics.timer.{ScheduleTimeout, Timeout}

case class BLETimeout(timeout: ScheduleTimeout) extends Timeout(timeout);
