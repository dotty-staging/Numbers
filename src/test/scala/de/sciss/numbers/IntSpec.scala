package de.sciss.numbers

import org.scalatest.{FlatSpec, Matchers}

class IntSpec extends FlatSpec with Matchers {
  def work: String = "produce the expected output"

  import IntFunctions.{signum => sign, _}
  import IntFunctions2.{roundTo => round, roundUpTo => roundUp, _}

  "Int.abs" should work in {
    abs(0) shouldBe 0
    abs(1) shouldBe 1
    abs(-1) shouldBe 1
    abs(10) shouldBe 10
    abs(-10) shouldBe 10
  }

  "Int.sign" should work in {
    sign(0) shouldBe 0
    sign(1) shouldBe 1
    sign(-1) shouldBe -1
    sign(10) shouldBe 1
    sign(-10) shouldBe -1
  }

  "Int.squared" should work in {
    squared(0) shouldBe 0
    squared(1) shouldBe 1
    squared(-1) shouldBe 1
    squared(10) shouldBe 100
    squared(-10) shouldBe 100
  }

  "Int.cubed" should work in {
    cubed(0) shouldBe 0
    cubed(1) shouldBe 1
    cubed(-1) shouldBe -1
    cubed(10) shouldBe 1000
    cubed(-10) shouldBe -1000
  }

  "Int.min" should work in {
    min(0, 0) shouldBe 0
    min(0, 1) shouldBe 0
    min(0, -1) shouldBe -1
    min(0, 6) shouldBe 0
    min(0, -6) shouldBe -6
    min(0, 11) shouldBe 0
    min(0, -11) shouldBe -11
    min(1, 0) shouldBe 0
    min(1, 1) shouldBe 1
    min(1, -1) shouldBe -1
    min(1, 6) shouldBe 1
    min(1, -6) shouldBe -6
    min(1, 11) shouldBe 1
    min(1, -11) shouldBe -11
    min(-1, 0) shouldBe -1
    min(-1, 1) shouldBe -1
    min(-1, -1) shouldBe -1
    min(-1, 6) shouldBe -1
    min(-1, -6) shouldBe -6
    min(-1, 11) shouldBe -1
    min(-1, -11) shouldBe -11
    min(10, 0) shouldBe 0
    min(10, 1) shouldBe 1
    min(10, -1) shouldBe -1
    min(10, 6) shouldBe 6
    min(10, -6) shouldBe -6
    min(10, 11) shouldBe 10
    min(10, -11) shouldBe -11
    min(-10, 0) shouldBe -10
    min(-10, 1) shouldBe -10
    min(-10, -1) shouldBe -10
    min(-10, 6) shouldBe -10
    min(-10, -6) shouldBe -10
    min(-10, 11) shouldBe -10
    min(-10, -11) shouldBe -11
  }

  "Int.max" should work in {
    max(0, 0) shouldBe 0
    max(0, 1) shouldBe 1
    max(0, -1) shouldBe 0
    max(0, 6) shouldBe 6
    max(0, -6) shouldBe 0
    max(0, 11) shouldBe 11
    max(0, -11) shouldBe 0
    max(1, 0) shouldBe 1
    max(1, 1) shouldBe 1
    max(1, -1) shouldBe 1
    max(1, 6) shouldBe 6
    max(1, -6) shouldBe 1
    max(1, 11) shouldBe 11
    max(1, -11) shouldBe 1
    max(-1, 0) shouldBe 0
    max(-1, 1) shouldBe 1
    max(-1, -1) shouldBe -1
    max(-1, 6) shouldBe 6
    max(-1, -6) shouldBe -1
    max(-1, 11) shouldBe 11
    max(-1, -11) shouldBe -1
    max(10, 0) shouldBe 10
    max(10, 1) shouldBe 10
    max(10, -1) shouldBe 10
    max(10, 6) shouldBe 10
    max(10, -6) shouldBe 10
    max(10, 11) shouldBe 11
    max(10, -11) shouldBe 10
    max(-10, 0) shouldBe 0
    max(-10, 1) shouldBe 1
    max(-10, -1) shouldBe -1
    max(-10, 6) shouldBe 6
    max(-10, -6) shouldBe -6
    max(-10, 11) shouldBe 11
    max(-10, -11) shouldBe -10
  }

  "Int.div" should work in {
    div(0, 0) shouldBe 0
    div(0, 1) shouldBe 0
    div(0, -1) shouldBe 0
    div(0, 6) shouldBe 0
    div(0, -6) shouldBe 0
    div(0, 11) shouldBe 0
    div(0, -11) shouldBe 0
    div(1, 0) shouldBe 1
    div(1, 1) shouldBe 1
    div(1, -1) shouldBe -1
    div(1, 6) shouldBe 0
    div(1, -6) shouldBe 0
    div(1, 11) shouldBe 0
    div(1, -11) shouldBe 0
    div(-1, 0) shouldBe -1
    div(-1, 1) shouldBe -1
    div(-1, -1) shouldBe -1
    div(-1, 6) shouldBe -1
    div(-1, -6) shouldBe -1
    div(-1, 11) shouldBe -1
    div(-1, -11) shouldBe -1
    div(10, 0) shouldBe 10
    div(10, 1) shouldBe 10
    div(10, -1) shouldBe -10
    div(10, 6) shouldBe 1
    div(10, -6) shouldBe -1
    div(10, 11) shouldBe 0
    div(10, -11) shouldBe 0
    div(-10, 0) shouldBe -10
    div(-10, 1) shouldBe -10
    div(-10, -1) shouldBe 8
    div(-10, 6) shouldBe -2
    div(-10, -6) shouldBe 0
    div(-10, 11) shouldBe -1
    div(-10, -11) shouldBe -1
  }

  "Int.gcd" should work in {
    gcd(0, 0) shouldBe 0
    gcd(0, 1) shouldBe 1
    gcd(0, -1) shouldBe -1
    gcd(0, 6) shouldBe 6
    gcd(0, -6) shouldBe -6
    gcd(0, 11) shouldBe 11
    gcd(0, -11) shouldBe -11
    gcd(1, 0) shouldBe 1
    gcd(1, 1) shouldBe 1
    gcd(1, -1) shouldBe 1
    gcd(1, 6) shouldBe 1
    gcd(1, -6) shouldBe 1
    gcd(1, 11) shouldBe 1
    gcd(1, -11) shouldBe 1
    gcd(-1, 0) shouldBe -1
    gcd(-1, 1) shouldBe 1
    gcd(-1, -1) shouldBe -1
    gcd(-1, 6) shouldBe 1
    gcd(-1, -6) shouldBe -1
    gcd(-1, 11) shouldBe 1
    gcd(-1, -11) shouldBe -1
    gcd(10, 0) shouldBe 10
    gcd(10, 1) shouldBe 1
    gcd(10, -1) shouldBe 1
    gcd(10, 6) shouldBe 2
    gcd(10, -6) shouldBe 2
    gcd(10, 11) shouldBe 1
    gcd(10, -11) shouldBe 1
    gcd(-10, 0) shouldBe -10
    gcd(-10, 1) shouldBe 1
    gcd(-10, -1) shouldBe -1
    gcd(-10, 6) shouldBe 2
    gcd(-10, -6) shouldBe -2
    gcd(-10, 11) shouldBe 1
    gcd(-10, -11) shouldBe -1
  }

  "Int.lcm" should work in {
    lcm(0, 0) shouldBe 0
    lcm(0, 1) shouldBe 0
    lcm(0, -1) shouldBe 0
    lcm(0, 6) shouldBe 0
    lcm(0, -6) shouldBe 0
    lcm(0, 11) shouldBe 0
    lcm(0, -11) shouldBe 0
    lcm(1, 0) shouldBe 0
    lcm(1, 1) shouldBe 1
    lcm(1, -1) shouldBe -1
    lcm(1, 6) shouldBe 6
    lcm(1, -6) shouldBe -6
    lcm(1, 11) shouldBe 11
    lcm(1, -11) shouldBe -11
    lcm(-1, 0) shouldBe 0
    lcm(-1, 1) shouldBe -1
    lcm(-1, -1) shouldBe -1
    lcm(-1, 6) shouldBe -6
    lcm(-1, -6) shouldBe -6
    lcm(-1, 11) shouldBe -11
    lcm(-1, -11) shouldBe -11
    lcm(10, 0) shouldBe 0
    lcm(10, 1) shouldBe 10
    lcm(10, -1) shouldBe -10
    lcm(10, 6) shouldBe 30
    lcm(10, -6) shouldBe -30
    lcm(10, 11) shouldBe 110
    lcm(10, -11) shouldBe -110
    lcm(-10, 0) shouldBe 0
    lcm(-10, 1) shouldBe -10
    lcm(-10, -1) shouldBe -10
    lcm(-10, 6) shouldBe -30
    lcm(-10, -6) shouldBe -30
    lcm(-10, 11) shouldBe -110
    lcm(-10, -11) shouldBe -110
  }

  "Int.clip2" should work in {
    clip2(0, 0) shouldBe 0
    clip2(0, 1) shouldBe 0
    clip2(0, -1) shouldBe 1
    clip2(0, 6) shouldBe 0
    clip2(0, -6) shouldBe 6
    clip2(0, 11) shouldBe 0
    clip2(0, -11) shouldBe 11
    clip2(1, 0) shouldBe 0
    clip2(1, 1) shouldBe 1
    clip2(1, -1) shouldBe 1
    clip2(1, 6) shouldBe 1
    clip2(1, -6) shouldBe 6
    clip2(1, 11) shouldBe 1
    clip2(1, -11) shouldBe 11
    clip2(-1, 0) shouldBe 0
    clip2(-1, 1) shouldBe -1
    clip2(-1, -1) shouldBe 1
    clip2(-1, 6) shouldBe -1
    clip2(-1, -6) shouldBe 6
    clip2(-1, 11) shouldBe -1
    clip2(-1, -11) shouldBe 11
    clip2(10, 0) shouldBe 0
    clip2(10, 1) shouldBe 1
    clip2(10, -1) shouldBe 1
    clip2(10, 6) shouldBe 6
    clip2(10, -6) shouldBe 6
    clip2(10, 11) shouldBe 10
    clip2(10, -11) shouldBe 11
    clip2(-10, 0) shouldBe 0
    clip2(-10, 1) shouldBe -1
    clip2(-10, -1) shouldBe 1
    clip2(-10, 6) shouldBe -6
    clip2(-10, -6) shouldBe 6
    clip2(-10, 11) shouldBe -10
    clip2(-10, -11) shouldBe 11
  }

  "Int.excess" should work in {
    excess(0, 0) shouldBe 0
    excess(0, 1) shouldBe 0
    excess(0, -1) shouldBe -1
    excess(0, 6) shouldBe 0
    excess(0, -6) shouldBe -6
    excess(0, 11) shouldBe 0
    excess(0, -11) shouldBe -11
    excess(1, 0) shouldBe 1
    excess(1, 1) shouldBe 0
    excess(1, -1) shouldBe 0
    excess(1, 6) shouldBe 0
    excess(1, -6) shouldBe -5
    excess(1, 11) shouldBe 0
    excess(1, -11) shouldBe -10
    excess(-1, 0) shouldBe -1
    excess(-1, 1) shouldBe 0
    excess(-1, -1) shouldBe -2
    excess(-1, 6) shouldBe 0
    excess(-1, -6) shouldBe -7
    excess(-1, 11) shouldBe 0
    excess(-1, -11) shouldBe -12
    excess(10, 0) shouldBe 10
    excess(10, 1) shouldBe 9
    excess(10, -1) shouldBe 9
    excess(10, 6) shouldBe 4
    excess(10, -6) shouldBe 4
    excess(10, 11) shouldBe 0
    excess(10, -11) shouldBe -1
    excess(-10, 0) shouldBe -10
    excess(-10, 1) shouldBe -9
    excess(-10, -1) shouldBe -11
    excess(-10, 6) shouldBe -4
    excess(-10, -6) shouldBe -16
    excess(-10, 11) shouldBe 0
    excess(-10, -11) shouldBe -21
  }

  "Int.fold2" should work in {
    fold2(0, 0) shouldBe 0
    fold2(0, 1) shouldBe 0
    fold2(0, -1) shouldBe -6
    fold2(0, 6) shouldBe 0
    fold2(0, -6) shouldBe -36
    fold2(0, 11) shouldBe 0
    fold2(0, -11) shouldBe -66
    fold2(1, 0) shouldBe 0
    fold2(1, 1) shouldBe 1
    fold2(1, -1) shouldBe -3
    fold2(1, 6) shouldBe 1
    fold2(1, -6) shouldBe -37
    fold2(1, 11) shouldBe 1
    fold2(1, -11) shouldBe -67
    fold2(-1, 0) shouldBe 0
    fold2(-1, 1) shouldBe -1
    fold2(-1, -1) shouldBe -5
    fold2(-1, 6) shouldBe -1
    fold2(-1, -6) shouldBe -35
    fold2(-1, 11) shouldBe -1
    fold2(-1, -11) shouldBe -65
    fold2(10, 0) shouldBe 0
    fold2(10, 1) shouldBe 0
    fold2(10, -1) shouldBe -4
    fold2(10, 6) shouldBe 2
    fold2(10, -6) shouldBe -22
    fold2(10, 11) shouldBe 10
    fold2(10, -11) shouldBe -76
    fold2(-10, 0) shouldBe 0
    fold2(-10, 1) shouldBe 0
    fold2(-10, -1) shouldBe -6
    fold2(-10, 6) shouldBe -2
    fold2(-10, -6) shouldBe -26
    fold2(-10, 11) shouldBe -10
    fold2(-10, -11) shouldBe -56
  }

  "Int.wrap2" should work in {
    wrap2(0, 0) shouldBe 0
    wrap2(0, 1) shouldBe 0
    wrap2(0, -1) shouldBe 1
    wrap2(0, 6) shouldBe 0
    wrap2(0, -6) shouldBe 11
    wrap2(0, 11) shouldBe 0
    wrap2(0, -11) shouldBe 21
    wrap2(1, 0) shouldBe 0
    wrap2(1, 1) shouldBe 1
    wrap2(1, -1) shouldBe 1
    wrap2(1, 6) shouldBe 1
    wrap2(1, -6) shouldBe 12
    wrap2(1, 11) shouldBe 1
    wrap2(1, -11) shouldBe 22
    wrap2(-1, 0) shouldBe 0
    wrap2(-1, 1) shouldBe -1
    wrap2(-1, -1) shouldBe 1
    wrap2(-1, 6) shouldBe -1
    wrap2(-1, -6) shouldBe 10
    wrap2(-1, 11) shouldBe -1
    wrap2(-1, -11) shouldBe 20
    wrap2(10, 0) shouldBe 0
    wrap2(10, 1) shouldBe 1
    wrap2(10, -1) shouldBe 1
    wrap2(10, 6) shouldBe -3
    wrap2(10, -6) shouldBe 10
    wrap2(10, 11) shouldBe 10
    wrap2(10, -11) shouldBe 31
    wrap2(-10, 0) shouldBe 0
    wrap2(-10, 1) shouldBe -1
    wrap2(-10, -1) shouldBe 1
    wrap2(-10, 6) shouldBe 3
    wrap2(-10, -6) shouldBe -10
    wrap2(-10, 11) shouldBe -10
    wrap2(-10, -11) shouldBe 11
  }

  "Int.mod" should work in {
    mod(0, 0) shouldBe 0
    mod(0, 1) shouldBe 0
    mod(0, -1) shouldBe 0
    mod(0, 6) shouldBe 0
    mod(0, -6) shouldBe 0
    mod(0, 11) shouldBe 0
    mod(0, -11) shouldBe 0
    mod(1, 0) shouldBe 0
    mod(1, 1) shouldBe 0
    mod(1, -1) shouldBe 0
    mod(1, 6) shouldBe 1
    mod(1, -6) shouldBe 1
    mod(1, 11) shouldBe 1
    mod(1, -11) shouldBe 1
    mod(-1, 0) shouldBe 0
    mod(-1, 1) shouldBe 0
    mod(-1, -1) shouldBe 0
    mod(-1, 6) shouldBe 5
    mod(-1, -6) shouldBe 5
    mod(-1, 11) shouldBe 10
    mod(-1, -11) shouldBe 10
    mod(10, 0) shouldBe 0
    mod(10, 1) shouldBe 0
    mod(10, -1) shouldBe 0
    mod(10, 6) shouldBe 4
    mod(10, -6) shouldBe 4
    mod(10, 11) shouldBe 10
    mod(10, -11) shouldBe 10
    mod(-10, 0) shouldBe 0
    mod(-10, 1) shouldBe 0
    mod(-10, -1) shouldBe 0
    mod(-10, 6) shouldBe 2
    mod(-10, -6) shouldBe -10
    mod(-10, 11) shouldBe 1
    mod(-10, -11) shouldBe 1
  }

  "Int.difsqr" should work in {
    difsqr(0, 0) shouldBe 0
    difsqr(0, 1) shouldBe -1
    difsqr(0, -1) shouldBe -1
    difsqr(0, 6) shouldBe -36
    difsqr(0, -6) shouldBe -36
    difsqr(0, 11) shouldBe -121
    difsqr(0, -11) shouldBe -121
    difsqr(1, 0) shouldBe 1
    difsqr(1, 1) shouldBe 0
    difsqr(1, -1) shouldBe 0
    difsqr(1, 6) shouldBe -35
    difsqr(1, -6) shouldBe -35
    difsqr(1, 11) shouldBe -120
    difsqr(1, -11) shouldBe -120
    difsqr(-1, 0) shouldBe 1
    difsqr(-1, 1) shouldBe 0
    difsqr(-1, -1) shouldBe 0
    difsqr(-1, 6) shouldBe -35
    difsqr(-1, -6) shouldBe -35
    difsqr(-1, 11) shouldBe -120
    difsqr(-1, -11) shouldBe -120
    difsqr(10, 0) shouldBe 100
    difsqr(10, 1) shouldBe 99
    difsqr(10, -1) shouldBe 99
    difsqr(10, 6) shouldBe 64
    difsqr(10, -6) shouldBe 64
    difsqr(10, 11) shouldBe -21
    difsqr(10, -11) shouldBe -21
    difsqr(-10, 0) shouldBe 100
    difsqr(-10, 1) shouldBe 99
    difsqr(-10, -1) shouldBe 99
    difsqr(-10, 6) shouldBe 64
    difsqr(-10, -6) shouldBe 64
    difsqr(-10, 11) shouldBe -21
    difsqr(-10, -11) shouldBe -21
  }

  "Int.sumsqr" should work in {
    sumsqr(0, 0) shouldBe 0
    sumsqr(0, 1) shouldBe 1
    sumsqr(0, -1) shouldBe 1
    sumsqr(0, 6) shouldBe 36
    sumsqr(0, -6) shouldBe 36
    sumsqr(0, 11) shouldBe 121
    sumsqr(0, -11) shouldBe 121
    sumsqr(1, 0) shouldBe 1
    sumsqr(1, 1) shouldBe 2
    sumsqr(1, -1) shouldBe 2
    sumsqr(1, 6) shouldBe 37
    sumsqr(1, -6) shouldBe 37
    sumsqr(1, 11) shouldBe 122
    sumsqr(1, -11) shouldBe 122
    sumsqr(-1, 0) shouldBe 1
    sumsqr(-1, 1) shouldBe 2
    sumsqr(-1, -1) shouldBe 2
    sumsqr(-1, 6) shouldBe 37
    sumsqr(-1, -6) shouldBe 37
    sumsqr(-1, 11) shouldBe 122
    sumsqr(-1, -11) shouldBe 122
    sumsqr(10, 0) shouldBe 100
    sumsqr(10, 1) shouldBe 101
    sumsqr(10, -1) shouldBe 101
    sumsqr(10, 6) shouldBe 136
    sumsqr(10, -6) shouldBe 136
    sumsqr(10, 11) shouldBe 221
    sumsqr(10, -11) shouldBe 221
    sumsqr(-10, 0) shouldBe 100
    sumsqr(-10, 1) shouldBe 101
    sumsqr(-10, -1) shouldBe 101
    sumsqr(-10, 6) shouldBe 136
    sumsqr(-10, -6) shouldBe 136
    sumsqr(-10, 11) shouldBe 221
    sumsqr(-10, -11) shouldBe 221
  }

  "Int.sqrsum" should work in {
    sqrsum(0, 0) shouldBe 0
    sqrsum(0, 1) shouldBe 1
    sqrsum(0, -1) shouldBe 1
    sqrsum(0, 6) shouldBe 36
    sqrsum(0, -6) shouldBe 36
    sqrsum(0, 11) shouldBe 121
    sqrsum(0, -11) shouldBe 121
    sqrsum(1, 0) shouldBe 1
    sqrsum(1, 1) shouldBe 4
    sqrsum(1, -1) shouldBe 0
    sqrsum(1, 6) shouldBe 49
    sqrsum(1, -6) shouldBe 25
    sqrsum(1, 11) shouldBe 144
    sqrsum(1, -11) shouldBe 100
    sqrsum(-1, 0) shouldBe 1
    sqrsum(-1, 1) shouldBe 0
    sqrsum(-1, -1) shouldBe 4
    sqrsum(-1, 6) shouldBe 25
    sqrsum(-1, -6) shouldBe 49
    sqrsum(-1, 11) shouldBe 100
    sqrsum(-1, -11) shouldBe 144
    sqrsum(10, 0) shouldBe 100
    sqrsum(10, 1) shouldBe 121
    sqrsum(10, -1) shouldBe 81
    sqrsum(10, 6) shouldBe 256
    sqrsum(10, -6) shouldBe 16
    sqrsum(10, 11) shouldBe 441
    sqrsum(10, -11) shouldBe 1
    sqrsum(-10, 0) shouldBe 100
    sqrsum(-10, 1) shouldBe 81
    sqrsum(-10, -1) shouldBe 121
    sqrsum(-10, 6) shouldBe 16
    sqrsum(-10, -6) shouldBe 256
    sqrsum(-10, 11) shouldBe 1
    sqrsum(-10, -11) shouldBe 441
  }

  "Int.sqrdif" should work in {
    sqrdif(0, 0) shouldBe 0
    sqrdif(0, 1) shouldBe 1
    sqrdif(0, -1) shouldBe 1
    sqrdif(0, 6) shouldBe 36
    sqrdif(0, -6) shouldBe 36
    sqrdif(0, 11) shouldBe 121
    sqrdif(0, -11) shouldBe 121
    sqrdif(1, 0) shouldBe 1
    sqrdif(1, 1) shouldBe 0
    sqrdif(1, -1) shouldBe 4
    sqrdif(1, 6) shouldBe 25
    sqrdif(1, -6) shouldBe 49
    sqrdif(1, 11) shouldBe 100
    sqrdif(1, -11) shouldBe 144
    sqrdif(-1, 0) shouldBe 1
    sqrdif(-1, 1) shouldBe 4
    sqrdif(-1, -1) shouldBe 0
    sqrdif(-1, 6) shouldBe 49
    sqrdif(-1, -6) shouldBe 25
    sqrdif(-1, 11) shouldBe 144
    sqrdif(-1, -11) shouldBe 100
    sqrdif(10, 0) shouldBe 100
    sqrdif(10, 1) shouldBe 81
    sqrdif(10, -1) shouldBe 121
    sqrdif(10, 6) shouldBe 16
    sqrdif(10, -6) shouldBe 256
    sqrdif(10, 11) shouldBe 1
    sqrdif(10, -11) shouldBe 441
    sqrdif(-10, 0) shouldBe 100
    sqrdif(-10, 1) shouldBe 121
    sqrdif(-10, -1) shouldBe 81
    sqrdif(-10, 6) shouldBe 256
    sqrdif(-10, -6) shouldBe 16
    sqrdif(-10, 11) shouldBe 441
    sqrdif(-10, -11) shouldBe 1
  }

  "Int.absdif" should work in {
    absdif(0, 0) shouldBe 0
    absdif(0, 1) shouldBe 1
    absdif(0, -1) shouldBe 1
    absdif(0, 6) shouldBe 6
    absdif(0, -6) shouldBe 6
    absdif(0, 11) shouldBe 11
    absdif(0, -11) shouldBe 11
    absdif(1, 0) shouldBe 1
    absdif(1, 1) shouldBe 0
    absdif(1, -1) shouldBe 2
    absdif(1, 6) shouldBe 5
    absdif(1, -6) shouldBe 7
    absdif(1, 11) shouldBe 10
    absdif(1, -11) shouldBe 12
    absdif(-1, 0) shouldBe 1
    absdif(-1, 1) shouldBe 2
    absdif(-1, -1) shouldBe 0
    absdif(-1, 6) shouldBe 7
    absdif(-1, -6) shouldBe 5
    absdif(-1, 11) shouldBe 12
    absdif(-1, -11) shouldBe 10
    absdif(10, 0) shouldBe 10
    absdif(10, 1) shouldBe 9
    absdif(10, -1) shouldBe 11
    absdif(10, 6) shouldBe 4
    absdif(10, -6) shouldBe 16
    absdif(10, 11) shouldBe 1
    absdif(10, -11) shouldBe 21
    absdif(-10, 0) shouldBe 10
    absdif(-10, 1) shouldBe 11
    absdif(-10, -1) shouldBe 9
    absdif(-10, 6) shouldBe 16
    absdif(-10, -6) shouldBe 4
    absdif(-10, 11) shouldBe 21
    absdif(-10, -11) shouldBe 1
  }

  "Int.round" should work in {
    round(0, 0) shouldBe 0
    round(0, 1) shouldBe 0
    round(0, -1) shouldBe 0
    round(0, 6) shouldBe 0
    round(0, -6) shouldBe 6
    round(0, 11) shouldBe 0
    round(0, -11) shouldBe 11
    round(1, 0) shouldBe 1
    round(1, 1) shouldBe 1
    round(1, -1) shouldBe 1
    round(1, 6) shouldBe 0
    round(1, -6) shouldBe 6
    round(1, 11) shouldBe 0
    round(1, -11) shouldBe 11
    round(-1, 0) shouldBe -1
    round(-1, 1) shouldBe -1
    round(-1, -1) shouldBe 1
    round(-1, 6) shouldBe 0
    round(-1, -6) shouldBe 6
    round(-1, 11) shouldBe 0
    round(-1, -11) shouldBe 11
    round(10, 0) shouldBe 10
    round(10, 1) shouldBe 10
    round(10, -1) shouldBe 10
    round(10, 6) shouldBe 12
    round(10, -6) shouldBe 6
    round(10, 11) shouldBe 11
    round(10, -11) shouldBe 0
    round(-10, 0) shouldBe -10
    round(-10, 1) shouldBe -10
    round(-10, -1) shouldBe -8
    round(-10, 6) shouldBe -12
    round(-10, -6) shouldBe -6
    round(-10, 11) shouldBe -11
    round(-10, -11) shouldBe 0
  }

  "Int.roundUp" should work in {
    roundUp(0, 0) shouldBe 0
    roundUp(0, 1) shouldBe 0
    roundUp(0, -1) shouldBe 0
    roundUp(0, 6) shouldBe 0
    roundUp(0, -6) shouldBe 0
    roundUp(0, 11) shouldBe 0
    roundUp(0, -11) shouldBe 0
    roundUp(1, 0) shouldBe 1
    roundUp(1, 1) shouldBe 1
    roundUp(1, -1) shouldBe 1
    roundUp(1, 6) shouldBe 6
    roundUp(1, -6) shouldBe 6
    roundUp(1, 11) shouldBe 11
    roundUp(1, -11) shouldBe 11
    roundUp(-1, 0) shouldBe -1
    roundUp(-1, 1) shouldBe -1
    roundUp(-1, -1) shouldBe -1
    roundUp(-1, 6) shouldBe 0
    roundUp(-1, -6) shouldBe 0
    roundUp(-1, 11) shouldBe 0
    roundUp(-1, -11) shouldBe 0
    roundUp(10, 0) shouldBe 10
    roundUp(10, 1) shouldBe 10
    roundUp(10, -1) shouldBe 8
    roundUp(10, 6) shouldBe 12
    roundUp(10, -6) shouldBe 0
    roundUp(10, 11) shouldBe 11
    roundUp(10, -11) shouldBe 11
    roundUp(-10, 0) shouldBe -10
    roundUp(-10, 1) shouldBe -10
    roundUp(-10, -1) shouldBe -10
    roundUp(-10, 6) shouldBe -6
    roundUp(-10, -6) shouldBe -6
    roundUp(-10, 11) shouldBe 0
    roundUp(-10, -11) shouldBe 0
  }

  "Int.trunc" should work in {
    trunc(0, 0) shouldBe 0
    trunc(0, 1) shouldBe 0
    trunc(0, -1) shouldBe 0
    trunc(0, 6) shouldBe 0
    trunc(0, -6) shouldBe 0
    trunc(0, 11) shouldBe 0
    trunc(0, -11) shouldBe 0
    trunc(1, 0) shouldBe 1
    trunc(1, 1) shouldBe 1
    trunc(1, -1) shouldBe 1
    trunc(1, 6) shouldBe 0
    trunc(1, -6) shouldBe 0
    trunc(1, 11) shouldBe 0
    trunc(1, -11) shouldBe 0
    trunc(-1, 0) shouldBe -1
    trunc(-1, 1) shouldBe -1
    trunc(-1, -1) shouldBe 1
    trunc(-1, 6) shouldBe -6
    trunc(-1, -6) shouldBe 6
    trunc(-1, 11) shouldBe -11
    trunc(-1, -11) shouldBe 11
    trunc(10, 0) shouldBe 10
    trunc(10, 1) shouldBe 10
    trunc(10, -1) shouldBe 10
    trunc(10, 6) shouldBe 6
    trunc(10, -6) shouldBe 6
    trunc(10, 11) shouldBe 0
    trunc(10, -11) shouldBe 0
    trunc(-10, 0) shouldBe -10
    trunc(-10, 1) shouldBe -10
    trunc(-10, -1) shouldBe -8
    trunc(-10, 6) shouldBe -12
    trunc(-10, -6) shouldBe 0
    trunc(-10, 11) shouldBe -11
    trunc(-10, -11) shouldBe 11
  }
}
