package de.sciss.numbers

import org.scalatest.{FlatSpec, Matchers}

class DoubleSpec extends FlatSpec with Matchers {
  def work: String = "produce the expected output"

  import DoubleFunctions.{signum => sign, roundTo => round, roundUpTo => roundUp, hypotApx => hypotApx, _}
  import DoubleFunctions2._

  final val eps: Double = 1.0e-5
  final val inf: Double = Double.PositiveInfinity

  "Double.abs" should work in {
    abs(0.0) shouldBe 0.0 +- eps
    abs(10.0) shouldBe 10.0 +- eps
    abs(10.1) shouldBe 10.1 +- eps
    abs(10.5) shouldBe 10.5 +- eps
    abs(10.9) shouldBe 10.9 +- eps
    abs(-10.0) shouldBe 10.0 +- eps
    abs(-10.1) shouldBe 10.1 +- eps
    abs(-10.5) shouldBe 10.5 +- eps
    abs(-10.9) shouldBe 10.9 +- eps
  }

  "Double.ceil" should work in {
    ceil(0.0) shouldBe 0.0 +- eps
    ceil(10.0) shouldBe 10.0 +- eps
    ceil(10.1) shouldBe 11.0 +- eps
    ceil(10.5) shouldBe 11.0 +- eps
    ceil(10.9) shouldBe 11.0 +- eps
    ceil(-10.0) shouldBe -10.0 +- eps
    ceil(-10.1) shouldBe -10.0 +- eps
    ceil(-10.5) shouldBe -10.0 +- eps
    ceil(-10.9) shouldBe -10.0 +- eps
  }

  "Double.floor" should work in {
    floor(0.0) shouldBe 0.0 +- eps
    floor(10.0) shouldBe 10.0 +- eps
    floor(10.1) shouldBe 10.0 +- eps
    floor(10.5) shouldBe 10.0 +- eps
    floor(10.9) shouldBe 10.0 +- eps
    floor(-10.0) shouldBe -10.0 +- eps
    floor(-10.1) shouldBe -11.0 +- eps
    floor(-10.5) shouldBe -11.0 +- eps
    floor(-10.9) shouldBe -11.0 +- eps
  }

  "Double.frac" should work in {
    frac(0.0) shouldBe 0.0 +- eps
    frac(10.0) shouldBe 0.0 +- eps
    frac(10.1) shouldBe 0.1 +- eps
    frac(10.5) shouldBe 0.5 +- eps
    frac(10.9) shouldBe 0.9 +- eps
    frac(-10.0) shouldBe 0.0 +- eps
    frac(-10.1) shouldBe 0.9 +- eps
    frac(-10.5) shouldBe 0.5 +- eps
    frac(-10.9) shouldBe 0.1 +- eps
  }

  "Double.sign" should work in {
    sign(0.0) shouldBe 0.0 +- eps
    sign(10.0) shouldBe 1.0 +- eps
    sign(10.1) shouldBe 1.0 +- eps
    sign(10.5) shouldBe 1.0 +- eps
    sign(10.9) shouldBe 1.0 +- eps
    sign(-10.0) shouldBe -1.0 +- eps
    sign(-10.1) shouldBe -1.0 +- eps
    sign(-10.5) shouldBe -1.0 +- eps
    sign(-10.9) shouldBe -1.0 +- eps
  }

  "Double.squared" should work in {
    squared(0.0) shouldBe 0.0 +- eps
    squared(10.0) shouldBe 100.0 +- eps
    squared(10.1) shouldBe 102.01 +- eps
    squared(10.5) shouldBe 110.25 +- eps
    squared(10.9) shouldBe 118.81 +- eps
    squared(-10.0) shouldBe 100.0 +- eps
    squared(-10.1) shouldBe 102.01 +- eps
    squared(-10.5) shouldBe 110.25 +- eps
    squared(-10.9) shouldBe 118.81 +- eps
  }

  "Double.sqrt" should work in {
    sqrt(0.0) shouldBe 0.0 +- eps
    sqrt(10.0) shouldBe 3.1622776601684 +- eps
    sqrt(10.1) shouldBe 3.1780497164141 +- eps
    sqrt(10.5) shouldBe 3.2403703492039 +- eps
    sqrt(10.9) shouldBe 3.3015148038438 +- eps
  }

  "Double.exp" should work in {
    exp(0.0) shouldBe 1.0 +- eps
    exp(10.0) shouldBe 22026.465794807 +- eps
    exp(10.1) shouldBe 24343.009424408 +- eps
    exp(10.5) shouldBe 36315.502674247 +- eps
    exp(10.9) shouldBe 54176.363796699 +- eps
    exp(-10.0) shouldBe 4.5399929762485e-05 +- eps
    exp(-10.1) shouldBe 4.1079555225301e-05 +- eps
    exp(-10.5) shouldBe 2.7536449349747e-05 +- eps
    exp(-10.9) shouldBe 1.8458233995781e-05 +- eps
  }

  "Double.midicps" should work in {
    midiCps(0.0) shouldBe 8.1757989156437 +- eps
    midiCps(10.0) shouldBe 14.56761754744 +- eps
    midiCps(10.1) shouldBe 14.652006897392 +- eps
    midiCps(10.5) shouldBe 14.994481324147 +- eps
    midiCps(10.9) shouldBe 15.344960711166 +- eps
    midiCps(-10.0) shouldBe 4.5885119987095 +- eps
    midiCps(-10.1) shouldBe 4.5620841142888 +- eps
    midiCps(-10.5) shouldBe 4.4578859691128 +- eps
    midiCps(-10.9) shouldBe 4.3560677128618 +- eps
  }

  "Double.cpsmidi" should work in {
    cpsMidi(0.0) shouldBe -inf +- eps
    cpsMidi(10.0) shouldBe 3.4868205763524 +- eps
    cpsMidi(10.1) shouldBe 3.6590840920773 +- eps
    cpsMidi(10.5) shouldBe 4.3314925110492 +- eps
    cpsMidi(10.9) shouldBe 4.9787581963788 +- eps
    cpsMidi(-10.0) shouldBe 3.4868205763524 +- eps
    cpsMidi(-10.1) shouldBe 3.6590840920773 +- eps
    cpsMidi(-10.5) shouldBe 4.3314925110492 +- eps
    cpsMidi(-10.9) shouldBe 4.9787581963788 +- eps
  }

  "Double.midiratio" should work in {
    midiRatio(0.0) shouldBe 1.0 +- eps
    midiRatio(10.0) shouldBe 1.7817974362766 +- eps
    midiRatio(10.1) shouldBe 1.7921192838197 +- eps
    midiRatio(10.5) shouldBe 1.8340080864049 +- eps
    midiRatio(10.9) shouldBe 1.8768759933376 +- eps
    midiRatio(-10.0) shouldBe 0.56123102415598 +- eps
    midiRatio(-10.1) shouldBe 0.55799857131641 +- eps
    midiRatio(-10.5) shouldBe 0.54525386633395 +- eps
    midiRatio(-10.9) shouldBe 0.53280025081558 +- eps
  }

  "Double.ratiomidi" should work in {
    ratioMidi(0.0) shouldBe -inf +- eps
    ratioMidi(10.0) shouldBe 39.863137138648 +- eps
    ratioMidi(10.1) shouldBe 40.035400654373 +- eps
    ratioMidi(10.5) shouldBe 40.707809073345 +- eps
    ratioMidi(10.9) shouldBe 41.355074758675 +- eps
    ratioMidi(-10.0) shouldBe 39.863137138648 +- eps
    ratioMidi(-10.1) shouldBe 40.035400654373 +- eps
    ratioMidi(-10.5) shouldBe 40.707809073345 +- eps
    ratioMidi(-10.9) shouldBe 41.355074758675 +- eps
  }

  "Double.dbamp" should work in {
    dbAmp(0.0) shouldBe 1.0 +- eps
    dbAmp(10.0) shouldBe 3.1622776601684 +- eps
    dbAmp(10.1) shouldBe 3.1988951096914 +- eps
    dbAmp(10.5) shouldBe 3.3496543915783 +- eps
    dbAmp(10.9) shouldBe 3.5075187395257 +- eps
    dbAmp(-10.0) shouldBe 0.31622776601684 +- eps
    dbAmp(-10.1) shouldBe 0.3126079367124 +- eps
    dbAmp(-10.5) shouldBe 0.2985382618918 +- eps
    dbAmp(-10.9) shouldBe 0.28510182675039 +- eps
  }

  "Double.ampdb" should work in {
    ampDb(0.0) shouldBe -inf +- eps
    ampDb(10.0) shouldBe 20.0 +- eps
    ampDb(10.1) shouldBe 20.086427475653 +- eps
    ampDb(10.5) shouldBe 20.423785981399 +- eps
    ampDb(10.9) shouldBe 20.748529958812 +- eps
  }

  "Double.octcps" should work in {
    octCps(0.0) shouldBe 16.351597831287 +- eps
    octCps(10.0) shouldBe 16744.036179238 +- eps
    octCps(10.1) shouldBe 17945.813632655 +- eps
    octCps(10.5) shouldBe 23679.643053545 +- eps
    octCps(10.9) shouldBe 31245.476333431 +- eps
    octCps(-10.0) shouldBe 0.015968357257117 +- eps
    octCps(-10.1) shouldBe 0.014899004141536 +- eps
    octCps(-10.5) shouldBe 0.011291333700917 +- eps
    octCps(-10.9) shouldBe 0.0085572307742382 +- eps
  }

  "Double.cpsoct" should work in {
    cpsOct(0.0) shouldBe -inf +- eps
    cpsOct(10.0) shouldBe -0.70943161865461 +- eps
    cpsOct(10.1) shouldBe -0.69507632567754 +- eps
    cpsOct(10.5) shouldBe -0.63904229076321 +- eps
    cpsOct(10.9) shouldBe -0.58510348365241 +- eps
    cpsOct(-10.0) shouldBe -0.70943161865461 +- eps
    cpsOct(-10.1) shouldBe -0.69507632567754 +- eps
    cpsOct(-10.5) shouldBe -0.63904229076321 +- eps
    cpsOct(-10.9) shouldBe -0.58510348365241 +- eps
  }

  "Double.log" should work in {
    log(0.0) shouldBe -inf +- eps
    log(10.0) shouldBe 2.302585092994 +- eps
    log(10.1) shouldBe 2.3125354238472 +- eps
    log(10.5) shouldBe 2.3513752571635 +- eps
    log(10.9) shouldBe 2.3887627892351 +- eps
  }

  "Double.log2" should work in {
    log2(0.0) shouldBe -inf +- eps
    log2(10.0) shouldBe 3.3219280948874 +- eps
    log2(10.1) shouldBe 3.3362833878644 +- eps
    log2(10.5) shouldBe 3.3923174227788 +- eps
    log2(10.9) shouldBe 3.4462562298896 +- eps
  }

  "Double.log10" should work in {
    log10(0.0) shouldBe -inf +- eps
    log10(10.0) shouldBe 1.0 +- eps
    log10(10.1) shouldBe 1.0043213737826 +- eps
    log10(10.5) shouldBe 1.0211892990699 +- eps
    log10(10.9) shouldBe 1.0374264979406 +- eps
  }

  "Double.sin" should work in {
    sin(0.0) shouldBe 0.0 +- eps
    sin(10.0) shouldBe -0.54402111088937 +- eps
    sin(10.1) shouldBe -0.62507064889288 +- eps
    sin(10.5) shouldBe -0.87969575997167 +- eps
    sin(10.9) shouldBe -0.99543625330638 +- eps
    sin(-10.0) shouldBe 0.54402111088937 +- eps
    sin(-10.1) shouldBe 0.62507064889288 +- eps
    sin(-10.5) shouldBe 0.87969575997167 +- eps
    sin(-10.9) shouldBe 0.99543625330638 +- eps
  }

  "Double.cos" should work in {
    cos(0.0) shouldBe 1.0 +- eps
    cos(10.0) shouldBe -0.83907152907645 +- eps
    cos(10.1) shouldBe -0.78056818016918 +- eps
    cos(10.5) shouldBe -0.47553692799599 +- eps
    cos(10.9) shouldBe -0.095428851000951 +- eps
    cos(-10.0) shouldBe -0.83907152907645 +- eps
    cos(-10.1) shouldBe -0.78056818016918 +- eps
    cos(-10.5) shouldBe -0.47553692799599 +- eps
    cos(-10.9) shouldBe -0.095428851000951 +- eps
  }

  "Double.tan" should work in {
    tan(0.0) shouldBe 0.0 +- eps
    tan(10.0) shouldBe 0.64836082745909 +- eps
    tan(10.1) shouldBe 0.80078930293751 +- eps
    tan(10.5) shouldBe 1.8498999934219 +- eps
    tan(10.9) shouldBe 10.431187663534 +- eps
    tan(-10.0) shouldBe -0.64836082745909 +- eps
    tan(-10.1) shouldBe -0.80078930293751 +- eps
    tan(-10.5) shouldBe -1.8498999934219 +- eps
    tan(-10.9) shouldBe -10.431187663534 +- eps
  }

  "Double.asin" should work in {
    asin(0.0) shouldBe 0.0 +- eps
  }

  "Double.acos" should work in {
    acos(0.0) shouldBe 1.5707963267949 +- eps
  }

  "Double.atan" should work in {
    atan(0.0) shouldBe 0.0 +- eps
    atan(10.0) shouldBe 1.4711276743037 +- eps
    atan(10.1) shouldBe 1.4721080661465 +- eps
    atan(10.5) shouldBe 1.4758446204521 +- eps
    atan(10.9) shouldBe 1.4793093099234 +- eps
    atan(-10.0) shouldBe -1.4711276743037 +- eps
    atan(-10.1) shouldBe -1.4721080661465 +- eps
    atan(-10.5) shouldBe -1.4758446204521 +- eps
    atan(-10.9) shouldBe -1.4793093099234 +- eps
  }

  "Double.sinh" should work in {
    sinh(0.0) shouldBe 0.0 +- eps
    sinh(10.0) shouldBe 11013.232874703 +- eps
    sinh(10.1) shouldBe 12171.504691664 +- eps
    sinh(10.5) shouldBe 18157.751323355 +- eps
    sinh(10.9) shouldBe 27088.18188912 +- eps
    sinh(-10.0) shouldBe -11013.232874703 +- eps
    sinh(-10.1) shouldBe -12171.504691664 +- eps
    sinh(-10.5) shouldBe -18157.751323355 +- eps
    sinh(-10.9) shouldBe -27088.18188912 +- eps
  }

  "Double.cosh" should work in {
    cosh(0.0) shouldBe 1.0 +- eps
    cosh(10.0) shouldBe 11013.232920103 +- eps
    cosh(10.1) shouldBe 12171.504732744 +- eps
    cosh(10.5) shouldBe 18157.751350892 +- eps
    cosh(10.9) shouldBe 27088.181907578 +- eps
    cosh(-10.0) shouldBe 11013.232920103 +- eps
    cosh(-10.1) shouldBe 12171.504732744 +- eps
    cosh(-10.5) shouldBe 18157.751350892 +- eps
    cosh(-10.9) shouldBe 27088.181907578 +- eps
  }

  "Double.tanh" should work in {
    tanh(0.0) shouldBe 0.0 +- eps
    tanh(10.0) shouldBe 0.99999999587769 +- eps
    tanh(10.1) shouldBe 0.99999999662494 +- eps
    tanh(10.5) shouldBe 0.99999999848349 +- eps
    tanh(10.9) shouldBe 0.99999999931859 +- eps
    tanh(-10.0) shouldBe -0.99999999587769 +- eps
    tanh(-10.1) shouldBe -0.99999999662494 +- eps
    tanh(-10.5) shouldBe -0.99999999848349 +- eps
    tanh(-10.9) shouldBe -0.99999999931859 +- eps
  }

  "Double.cubed" should work in {
    cubed(0.0) shouldBe 0.0 +- eps
    cubed(10.0) shouldBe 1000.0 +- eps
    cubed(10.1) shouldBe 1030.301 +- eps
    cubed(10.5) shouldBe 1157.625 +- eps
    cubed(10.9) shouldBe 1295.029 +- eps
    cubed(-10.0) shouldBe -1000.0 +- eps
    cubed(-10.1) shouldBe -1030.301 +- eps
    cubed(-10.5) shouldBe -1157.625 +- eps
    cubed(-10.9) shouldBe -1295.029 +- eps
  }

  "Double.reciprocal" should work in {
    reciprocal(0.0) shouldBe inf +- eps
    reciprocal(10.0) shouldBe 0.1 +- eps
    reciprocal(10.1) shouldBe 0.099009900990099 +- eps
    reciprocal(10.5) shouldBe 0.095238095238095 +- eps
    reciprocal(10.9) shouldBe 0.091743119266055 +- eps
    reciprocal(-10.0) shouldBe -0.1 +- eps
    reciprocal(-10.1) shouldBe -0.099009900990099 +- eps
    reciprocal(-10.5) shouldBe -0.095238095238095 +- eps
    reciprocal(-10.9) shouldBe -0.091743119266055 +- eps
  }

  "Double.div" should work in {
    div(0.0, 0.0) shouldBe 0
    div(0.0, 6.0) shouldBe 0
    div(0.0, 6.1) shouldBe 0
    div(0.0, 6.5) shouldBe 0
    div(0.0, 6.9) shouldBe 0
    div(0.0, -6.0) shouldBe 0
    div(0.0, -6.1) shouldBe 0
    div(0.0, -6.5) shouldBe 0
    div(0.0, -6.9) shouldBe 0
    div(10.0, 0.0) shouldBe 0
    div(10.0, 6.0) shouldBe 1
    div(10.0, 6.1) shouldBe 1
    div(10.0, 6.5) shouldBe 1
    div(10.0, 6.9) shouldBe 1
    div(10.0, -6.0) shouldBe -2
    div(10.0, -6.1) shouldBe -2
    div(10.0, -6.5) shouldBe -2
    div(10.0, -6.9) shouldBe -2
    div(10.1, 0.0) shouldBe 0
    div(10.1, 6.0) shouldBe 1
    div(10.1, 6.1) shouldBe 1
    div(10.1, 6.5) shouldBe 1
    div(10.1, 6.9) shouldBe 1
    div(10.1, -6.0) shouldBe -2
    div(10.1, -6.1) shouldBe -2
    div(10.1, -6.5) shouldBe -2
    div(10.1, -6.9) shouldBe -2
    div(10.5, 0.0) shouldBe 0
    div(10.5, 6.0) shouldBe 1
    div(10.5, 6.1) shouldBe 1
    div(10.5, 6.5) shouldBe 1
    div(10.5, 6.9) shouldBe 1
    div(10.5, -6.0) shouldBe -2
    div(10.5, -6.1) shouldBe -2
    div(10.5, -6.5) shouldBe -2
    div(10.5, -6.9) shouldBe -2
    div(10.9, 0.0) shouldBe 0
    div(10.9, 6.0) shouldBe 1
    div(10.9, 6.1) shouldBe 1
    div(10.9, 6.5) shouldBe 1
    div(10.9, 6.9) shouldBe 1
    div(10.9, -6.0) shouldBe -2
    div(10.9, -6.1) shouldBe -2
    div(10.9, -6.5) shouldBe -2
    div(10.9, -6.9) shouldBe -2
    div(-10.0, 0.0) shouldBe 0
    div(-10.0, 6.0) shouldBe -2
    div(-10.0, 6.1) shouldBe -2
    div(-10.0, 6.5) shouldBe -2
    div(-10.0, 6.9) shouldBe -2
    div(-10.0, -6.0) shouldBe 1
    div(-10.0, -6.1) shouldBe 1
    div(-10.0, -6.5) shouldBe 1
    div(-10.0, -6.9) shouldBe 1
    div(-10.1, 0.0) shouldBe 0
    div(-10.1, 6.0) shouldBe -2
    div(-10.1, 6.1) shouldBe -2
    div(-10.1, 6.5) shouldBe -2
    div(-10.1, 6.9) shouldBe -2
    div(-10.1, -6.0) shouldBe 1
    div(-10.1, -6.1) shouldBe 1
    div(-10.1, -6.5) shouldBe 1
    div(-10.1, -6.9) shouldBe 1
    div(-10.5, 0.0) shouldBe 0
    div(-10.5, 6.0) shouldBe -2
    div(-10.5, 6.1) shouldBe -2
    div(-10.5, 6.5) shouldBe -2
    div(-10.5, 6.9) shouldBe -2
    div(-10.5, -6.0) shouldBe 1
    div(-10.5, -6.1) shouldBe 1
    div(-10.5, -6.5) shouldBe 1
    div(-10.5, -6.9) shouldBe 1
    div(-10.9, 0.0) shouldBe 0
    div(-10.9, 6.0) shouldBe -2
    div(-10.9, 6.1) shouldBe -2
    div(-10.9, 6.5) shouldBe -2
    div(-10.9, 6.9) shouldBe -2
    div(-10.9, -6.0) shouldBe 1
    div(-10.9, -6.1) shouldBe 1
    div(-10.9, -6.5) shouldBe 1
    div(-10.9, -6.9) shouldBe 1
  }

  "Double.round" should work in {
    round(0.0, 0.0) shouldBe 0.0 +- eps
    round(0.0, 6.0) shouldBe 0.0 +- eps
    round(0.0, 6.1) shouldBe 0.0 +- eps
    round(0.0, 6.5) shouldBe 0.0 +- eps
    round(0.0, 6.9) shouldBe 0.0 +- eps
    round(0.0, -6.0) shouldBe -0.0 +- eps
    round(0.0, -6.1) shouldBe -0.0 +- eps
    round(0.0, -6.5) shouldBe -0.0 +- eps
    round(0.0, -6.9) shouldBe -0.0 +- eps
    round(10.0, 0.0) shouldBe 10.0 +- eps
    round(10.0, 6.0) shouldBe 12.0 +- eps
    round(10.0, 6.1) shouldBe 12.2 +- eps
    round(10.0, 6.5) shouldBe 13.0 +- eps
    round(10.0, 6.9) shouldBe 6.9 +- eps
    round(10.0, -6.0) shouldBe 12.0 +- eps
    round(10.0, -6.1) shouldBe 12.2 +- eps
    round(10.0, -6.5) shouldBe 13.0 +- eps
    round(10.0, -6.9) shouldBe 6.9 +- eps
    round(10.1, 0.0) shouldBe 10.1 +- eps
    round(10.1, 6.0) shouldBe 12.0 +- eps
    round(10.1, 6.1) shouldBe 12.2 +- eps
    round(10.1, 6.5) shouldBe 13.0 +- eps
    round(10.1, 6.9) shouldBe 6.9 +- eps
    round(10.1, -6.0) shouldBe 12.0 +- eps
    round(10.1, -6.1) shouldBe 12.2 +- eps
    round(10.1, -6.5) shouldBe 13.0 +- eps
    round(10.1, -6.9) shouldBe 6.9 +- eps
    round(10.5, 0.0) shouldBe 10.5 +- eps
    round(10.5, 6.0) shouldBe 12.0 +- eps
    round(10.5, 6.1) shouldBe 12.2 +- eps
    round(10.5, 6.5) shouldBe 13.0 +- eps
    round(10.5, 6.9) shouldBe 13.8 +- eps
    round(10.5, -6.0) shouldBe 12.0 +- eps
    round(10.5, -6.1) shouldBe 12.2 +- eps
    round(10.5, -6.5) shouldBe 13.0 +- eps
    round(10.5, -6.9) shouldBe 13.8 +- eps
    round(10.9, 0.0) shouldBe 10.9 +- eps
    round(10.9, 6.0) shouldBe 12.0 +- eps
    round(10.9, 6.1) shouldBe 12.2 +- eps
    round(10.9, 6.5) shouldBe 13.0 +- eps
    round(10.9, 6.9) shouldBe 13.8 +- eps
    round(10.9, -6.0) shouldBe 12.0 +- eps
    round(10.9, -6.1) shouldBe 12.2 +- eps
    round(10.9, -6.5) shouldBe 13.0 +- eps
    round(10.9, -6.9) shouldBe 13.8 +- eps
    round(-10.0, 0.0) shouldBe -10.0 +- eps
    round(-10.0, 6.0) shouldBe -12.0 +- eps
    round(-10.0, 6.1) shouldBe -12.2 +- eps
    round(-10.0, 6.5) shouldBe -13.0 +- eps
    round(-10.0, 6.9) shouldBe -6.9 +- eps
    round(-10.0, -6.0) shouldBe -12.0 +- eps
    round(-10.0, -6.1) shouldBe -12.2 +- eps
    round(-10.0, -6.5) shouldBe -13.0 +- eps
    round(-10.0, -6.9) shouldBe -6.9 +- eps
    round(-10.1, 0.0) shouldBe -10.1 +- eps
    round(-10.1, 6.0) shouldBe -12.0 +- eps
    round(-10.1, 6.1) shouldBe -12.2 +- eps
    round(-10.1, 6.5) shouldBe -13.0 +- eps
    round(-10.1, 6.9) shouldBe -6.9 +- eps
    round(-10.1, -6.0) shouldBe -12.0 +- eps
    round(-10.1, -6.1) shouldBe -12.2 +- eps
    round(-10.1, -6.5) shouldBe -13.0 +- eps
    round(-10.1, -6.9) shouldBe -6.9 +- eps
    round(-10.5, 0.0) shouldBe -10.5 +- eps
    round(-10.5, 6.0) shouldBe -12.0 +- eps
    round(-10.5, 6.1) shouldBe -12.2 +- eps
    round(-10.5, 6.5) shouldBe -13.0 +- eps
    round(-10.5, 6.9) shouldBe -13.8 +- eps
    round(-10.5, -6.0) shouldBe -12.0 +- eps
    round(-10.5, -6.1) shouldBe -12.2 +- eps
    round(-10.5, -6.5) shouldBe -13.0 +- eps
    round(-10.5, -6.9) shouldBe -13.8 +- eps
    round(-10.9, 0.0) shouldBe -10.9 +- eps
    round(-10.9, 6.0) shouldBe -12.0 +- eps
    round(-10.9, 6.1) shouldBe -12.2 +- eps
    round(-10.9, 6.5) shouldBe -13.0 +- eps
    round(-10.9, 6.9) shouldBe -13.8 +- eps
    round(-10.9, -6.0) shouldBe -12.0 +- eps
    round(-10.9, -6.1) shouldBe -12.2 +- eps
    round(-10.9, -6.5) shouldBe -13.0 +- eps
    round(-10.9, -6.9) shouldBe -13.8 +- eps
  }

  "Double.roundUp" should work in {
    roundUp(0.0, 0.0) shouldBe 0.0 +- eps
    roundUp(0.0, 6.0) shouldBe 0.0 +- eps
    roundUp(0.0, 6.1) shouldBe 0.0 +- eps
    roundUp(0.0, 6.5) shouldBe 0.0 +- eps
    roundUp(0.0, 6.9) shouldBe 0.0 +- eps
    roundUp(0.0, -6.0) shouldBe 0.0 +- eps
    roundUp(0.0, -6.1) shouldBe 0.0 +- eps
    roundUp(0.0, -6.5) shouldBe 0.0 +- eps
    roundUp(0.0, -6.9) shouldBe 0.0 +- eps
    roundUp(10.0, 0.0) shouldBe 10.0 +- eps
    roundUp(10.0, 6.0) shouldBe 12.0 +- eps
    roundUp(10.0, 6.1) shouldBe 12.2 +- eps
    roundUp(10.0, 6.5) shouldBe 13.0 +- eps
    roundUp(10.0, 6.9) shouldBe 13.8 +- eps
    roundUp(10.0, -6.0) shouldBe 6.0 +- eps
    roundUp(10.0, -6.1) shouldBe 6.1 +- eps
    roundUp(10.0, -6.5) shouldBe 6.5 +- eps
    roundUp(10.0, -6.9) shouldBe 6.9 +- eps
    roundUp(10.1, 0.0) shouldBe 10.1 +- eps
    roundUp(10.1, 6.0) shouldBe 12.0 +- eps
    roundUp(10.1, 6.1) shouldBe 12.2 +- eps
    roundUp(10.1, 6.5) shouldBe 13.0 +- eps
    roundUp(10.1, 6.9) shouldBe 13.8 +- eps
    roundUp(10.1, -6.0) shouldBe 6.0 +- eps
    roundUp(10.1, -6.1) shouldBe 6.1 +- eps
    roundUp(10.1, -6.5) shouldBe 6.5 +- eps
    roundUp(10.1, -6.9) shouldBe 6.9 +- eps
    roundUp(10.5, 0.0) shouldBe 10.5 +- eps
    roundUp(10.5, 6.0) shouldBe 12.0 +- eps
    roundUp(10.5, 6.1) shouldBe 12.2 +- eps
    roundUp(10.5, 6.5) shouldBe 13.0 +- eps
    roundUp(10.5, 6.9) shouldBe 13.8 +- eps
    roundUp(10.5, -6.0) shouldBe 6.0 +- eps
    roundUp(10.5, -6.1) shouldBe 6.1 +- eps
    roundUp(10.5, -6.5) shouldBe 6.5 +- eps
    roundUp(10.5, -6.9) shouldBe 6.9 +- eps
    roundUp(10.9, 0.0) shouldBe 10.9 +- eps
    roundUp(10.9, 6.0) shouldBe 12.0 +- eps
    roundUp(10.9, 6.1) shouldBe 12.2 +- eps
    roundUp(10.9, 6.5) shouldBe 13.0 +- eps
    roundUp(10.9, 6.9) shouldBe 13.8 +- eps
    roundUp(10.9, -6.0) shouldBe 6.0 +- eps
    roundUp(10.9, -6.1) shouldBe 6.1 +- eps
    roundUp(10.9, -6.5) shouldBe 6.5 +- eps
    roundUp(10.9, -6.9) shouldBe 6.9 +- eps
    roundUp(-10.0, 0.0) shouldBe -10.0 +- eps
    roundUp(-10.0, 6.0) shouldBe -6.0 +- eps
    roundUp(-10.0, 6.1) shouldBe -6.1 +- eps
    roundUp(-10.0, 6.5) shouldBe -6.5 +- eps
    roundUp(-10.0, 6.9) shouldBe -6.9 +- eps
    roundUp(-10.0, -6.0) shouldBe -12.0 +- eps
    roundUp(-10.0, -6.1) shouldBe -12.2 +- eps
    roundUp(-10.0, -6.5) shouldBe -13.0 +- eps
    roundUp(-10.0, -6.9) shouldBe -13.8 +- eps
    roundUp(-10.1, 0.0) shouldBe -10.1 +- eps
    roundUp(-10.1, 6.0) shouldBe -6.0 +- eps
    roundUp(-10.1, 6.1) shouldBe -6.1 +- eps
    roundUp(-10.1, 6.5) shouldBe -6.5 +- eps
    roundUp(-10.1, 6.9) shouldBe -6.9 +- eps
    roundUp(-10.1, -6.0) shouldBe -12.0 +- eps
    roundUp(-10.1, -6.1) shouldBe -12.2 +- eps
    roundUp(-10.1, -6.5) shouldBe -13.0 +- eps
    roundUp(-10.1, -6.9) shouldBe -13.8 +- eps
    roundUp(-10.5, 0.0) shouldBe -10.5 +- eps
    roundUp(-10.5, 6.0) shouldBe -6.0 +- eps
    roundUp(-10.5, 6.1) shouldBe -6.1 +- eps
    roundUp(-10.5, 6.5) shouldBe -6.5 +- eps
    roundUp(-10.5, 6.9) shouldBe -6.9 +- eps
    roundUp(-10.5, -6.0) shouldBe -12.0 +- eps
    roundUp(-10.5, -6.1) shouldBe -12.2 +- eps
    roundUp(-10.5, -6.5) shouldBe -13.0 +- eps
    roundUp(-10.5, -6.9) shouldBe -13.8 +- eps
    roundUp(-10.9, 0.0) shouldBe -10.9 +- eps
    roundUp(-10.9, 6.0) shouldBe -6.0 +- eps
    roundUp(-10.9, 6.1) shouldBe -6.1 +- eps
    roundUp(-10.9, 6.5) shouldBe -6.5 +- eps
    roundUp(-10.9, 6.9) shouldBe -6.9 +- eps
    roundUp(-10.9, -6.0) shouldBe -12.0 +- eps
    roundUp(-10.9, -6.1) shouldBe -12.2 +- eps
    roundUp(-10.9, -6.5) shouldBe -13.0 +- eps
    roundUp(-10.9, -6.9) shouldBe -13.8 +- eps
  }

  "Double.trunc" should work in {
    trunc(0.0, 0.0) shouldBe 0.0 +- eps
    trunc(0.0, 6.0) shouldBe 0.0 +- eps
    trunc(0.0, 6.1) shouldBe 0.0 +- eps
    trunc(0.0, 6.5) shouldBe 0.0 +- eps
    trunc(0.0, 6.9) shouldBe 0.0 +- eps
    trunc(0.0, -6.0) shouldBe 0.0 +- eps
    trunc(0.0, -6.1) shouldBe 0.0 +- eps
    trunc(0.0, -6.5) shouldBe 0.0 +- eps
    trunc(0.0, -6.9) shouldBe 0.0 +- eps
    trunc(10.0, 0.0) shouldBe 10.0 +- eps
    trunc(10.0, 6.0) shouldBe 6.0 +- eps
    trunc(10.0, 6.1) shouldBe 6.1 +- eps
    trunc(10.0, 6.5) shouldBe 6.5 +- eps
    trunc(10.0, 6.9) shouldBe 6.9 +- eps
    trunc(10.0, -6.0) shouldBe 12.0 +- eps
    trunc(10.0, -6.1) shouldBe 12.2 +- eps
    trunc(10.0, -6.5) shouldBe 13.0 +- eps
    trunc(10.0, -6.9) shouldBe 13.8 +- eps
    trunc(10.1, 0.0) shouldBe 10.1 +- eps
    trunc(10.1, 6.0) shouldBe 6.0 +- eps
    trunc(10.1, 6.1) shouldBe 6.1 +- eps
    trunc(10.1, 6.5) shouldBe 6.5 +- eps
    trunc(10.1, 6.9) shouldBe 6.9 +- eps
    trunc(10.1, -6.0) shouldBe 12.0 +- eps
    trunc(10.1, -6.1) shouldBe 12.2 +- eps
    trunc(10.1, -6.5) shouldBe 13.0 +- eps
    trunc(10.1, -6.9) shouldBe 13.8 +- eps
    trunc(10.5, 0.0) shouldBe 10.5 +- eps
    trunc(10.5, 6.0) shouldBe 6.0 +- eps
    trunc(10.5, 6.1) shouldBe 6.1 +- eps
    trunc(10.5, 6.5) shouldBe 6.5 +- eps
    trunc(10.5, 6.9) shouldBe 6.9 +- eps
    trunc(10.5, -6.0) shouldBe 12.0 +- eps
    trunc(10.5, -6.1) shouldBe 12.2 +- eps
    trunc(10.5, -6.5) shouldBe 13.0 +- eps
    trunc(10.5, -6.9) shouldBe 13.8 +- eps
    trunc(10.9, 0.0) shouldBe 10.9 +- eps
    trunc(10.9, 6.0) shouldBe 6.0 +- eps
    trunc(10.9, 6.1) shouldBe 6.1 +- eps
    trunc(10.9, 6.5) shouldBe 6.5 +- eps
    trunc(10.9, 6.9) shouldBe 6.9 +- eps
    trunc(10.9, -6.0) shouldBe 12.0 +- eps
    trunc(10.9, -6.1) shouldBe 12.2 +- eps
    trunc(10.9, -6.5) shouldBe 13.0 +- eps
    trunc(10.9, -6.9) shouldBe 13.8 +- eps
    trunc(-10.0, 0.0) shouldBe -10.0 +- eps
    trunc(-10.0, 6.0) shouldBe -12.0 +- eps
    trunc(-10.0, 6.1) shouldBe -12.2 +- eps
    trunc(-10.0, 6.5) shouldBe -13.0 +- eps
    trunc(-10.0, 6.9) shouldBe -13.8 +- eps
    trunc(-10.0, -6.0) shouldBe -6.0 +- eps
    trunc(-10.0, -6.1) shouldBe -6.1 +- eps
    trunc(-10.0, -6.5) shouldBe -6.5 +- eps
    trunc(-10.0, -6.9) shouldBe -6.9 +- eps
    trunc(-10.1, 0.0) shouldBe -10.1 +- eps
    trunc(-10.1, 6.0) shouldBe -12.0 +- eps
    trunc(-10.1, 6.1) shouldBe -12.2 +- eps
    trunc(-10.1, 6.5) shouldBe -13.0 +- eps
    trunc(-10.1, 6.9) shouldBe -13.8 +- eps
    trunc(-10.1, -6.0) shouldBe -6.0 +- eps
    trunc(-10.1, -6.1) shouldBe -6.1 +- eps
    trunc(-10.1, -6.5) shouldBe -6.5 +- eps
    trunc(-10.1, -6.9) shouldBe -6.9 +- eps
    trunc(-10.5, 0.0) shouldBe -10.5 +- eps
    trunc(-10.5, 6.0) shouldBe -12.0 +- eps
    trunc(-10.5, 6.1) shouldBe -12.2 +- eps
    trunc(-10.5, 6.5) shouldBe -13.0 +- eps
    trunc(-10.5, 6.9) shouldBe -13.8 +- eps
    trunc(-10.5, -6.0) shouldBe -6.0 +- eps
    trunc(-10.5, -6.1) shouldBe -6.1 +- eps
    trunc(-10.5, -6.5) shouldBe -6.5 +- eps
    trunc(-10.5, -6.9) shouldBe -6.9 +- eps
    trunc(-10.9, 0.0) shouldBe -10.9 +- eps
    trunc(-10.9, 6.0) shouldBe -12.0 +- eps
    trunc(-10.9, 6.1) shouldBe -12.2 +- eps
    trunc(-10.9, 6.5) shouldBe -13.0 +- eps
    trunc(-10.9, 6.9) shouldBe -13.8 +- eps
    trunc(-10.9, -6.0) shouldBe -6.0 +- eps
    trunc(-10.9, -6.1) shouldBe -6.1 +- eps
    trunc(-10.9, -6.5) shouldBe -6.5 +- eps
    trunc(-10.9, -6.9) shouldBe -6.9 +- eps
  }

  "Double.atan2" should work in {
    atan2(0.0, 0.0) shouldBe 0.0 +- eps
    atan2(0.0, 6.0) shouldBe 0.0 +- eps
    atan2(0.0, 6.1) shouldBe 0.0 +- eps
    atan2(0.0, 6.5) shouldBe 0.0 +- eps
    atan2(0.0, 6.9) shouldBe 0.0 +- eps
    atan2(0.0, -6.0) shouldBe 3.1415926535898 +- eps
    atan2(0.0, -6.1) shouldBe 3.1415926535898 +- eps
    atan2(0.0, -6.5) shouldBe 3.1415926535898 +- eps
    atan2(0.0, -6.9) shouldBe 3.1415926535898 +- eps
    atan2(10.0, 0.0) shouldBe 1.5707963267949 +- eps
    atan2(10.0, 6.0) shouldBe 1.0303768265243 +- eps
    atan2(10.0, 6.1) shouldBe 1.023056313079 +- eps
    atan2(10.0, 6.5) shouldBe 0.99442110620371 +- eps
    atan2(10.0, 6.9) shouldBe 0.9668133485419 +- eps
    atan2(10.0, -6.0) shouldBe 2.1112158270655 +- eps
    atan2(10.0, -6.1) shouldBe 2.1185363405108 +- eps
    atan2(10.0, -6.5) shouldBe 2.1471715473861 +- eps
    atan2(10.0, -6.9) shouldBe 2.1747793050479 +- eps
    atan2(10.1, 0.0) shouldBe 1.5707963267949 +- eps
    atan2(10.1, 6.0) shouldBe 1.0347563605676 +- eps
    atan2(10.1, 6.1) shouldBe 1.0274698579686 +- eps
    atan2(10.1, 6.5) shouldBe 0.99895859687794 +- eps
    atan2(10.1, 6.9) shouldBe 0.97145634054645 +- eps
    atan2(10.1, -6.0) shouldBe 2.1068362930222 +- eps
    atan2(10.1, -6.1) shouldBe 2.1141227956212 +- eps
    atan2(10.1, -6.5) shouldBe 2.1426340567119 +- eps
    atan2(10.1, -6.9) shouldBe 2.1701363130433 +- eps
    atan2(10.5, 0.0) shouldBe 1.5707963267949 +- eps
    atan2(10.5, 6.0) shouldBe 1.0516502125484 +- eps
    atan2(10.5, 6.1) shouldBe 1.0445001811769 +- eps
    atan2(10.5, 6.5) shouldBe 1.0164888305933 +- eps
    atan2(10.5, 6.9) shouldBe 0.98941614299 +- eps
    atan2(10.5, -6.0) shouldBe 2.0899424410414 +- eps
    atan2(10.5, -6.1) shouldBe 2.0970924724129 +- eps
    atan2(10.5, -6.5) shouldBe 2.1251038229964 +- eps
    atan2(10.5, -6.9) shouldBe 2.1521765105998 +- eps
    atan2(10.9, 0.0) shouldBe 1.5707963267949 +- eps
    atan2(10.9, 6.0) shouldBe 1.0676010032055 +- eps
    atan2(10.9, 6.1) shouldBe 1.0605874125266 +- eps
    atan2(10.9, 6.5) shouldBe 1.0330795226463 +- eps
    atan2(10.9, 6.9) shouldBe 1.0064452260725 +- eps
    atan2(10.9, -6.0) shouldBe 2.0739916503843 +- eps
    atan2(10.9, -6.1) shouldBe 2.0810052410632 +- eps
    atan2(10.9, -6.5) shouldBe 2.1085131309435 +- eps
    atan2(10.9, -6.9) shouldBe 2.1351474275173 +- eps
    atan2(-10.0, 0.0) shouldBe -1.5707963267949 +- eps
    atan2(-10.0, 6.0) shouldBe -1.0303768265243 +- eps
    atan2(-10.0, 6.1) shouldBe -1.023056313079 +- eps
    atan2(-10.0, 6.5) shouldBe -0.99442110620371 +- eps
    atan2(-10.0, 6.9) shouldBe -0.9668133485419 +- eps
    atan2(-10.0, -6.0) shouldBe -2.1112158270655 +- eps
    atan2(-10.0, -6.1) shouldBe -2.1185363405108 +- eps
    atan2(-10.0, -6.5) shouldBe -2.1471715473861 +- eps
    atan2(-10.0, -6.9) shouldBe -2.1747793050479 +- eps
    atan2(-10.1, 0.0) shouldBe -1.5707963267949 +- eps
    atan2(-10.1, 6.0) shouldBe -1.0347563605676 +- eps
    atan2(-10.1, 6.1) shouldBe -1.0274698579686 +- eps
    atan2(-10.1, 6.5) shouldBe -0.99895859687794 +- eps
    atan2(-10.1, 6.9) shouldBe -0.97145634054645 +- eps
    atan2(-10.1, -6.0) shouldBe -2.1068362930222 +- eps
    atan2(-10.1, -6.1) shouldBe -2.1141227956212 +- eps
    atan2(-10.1, -6.5) shouldBe -2.1426340567119 +- eps
    atan2(-10.1, -6.9) shouldBe -2.1701363130433 +- eps
    atan2(-10.5, 0.0) shouldBe -1.5707963267949 +- eps
    atan2(-10.5, 6.0) shouldBe -1.0516502125484 +- eps
    atan2(-10.5, 6.1) shouldBe -1.0445001811769 +- eps
    atan2(-10.5, 6.5) shouldBe -1.0164888305933 +- eps
    atan2(-10.5, 6.9) shouldBe -0.98941614299 +- eps
    atan2(-10.5, -6.0) shouldBe -2.0899424410414 +- eps
    atan2(-10.5, -6.1) shouldBe -2.0970924724129 +- eps
    atan2(-10.5, -6.5) shouldBe -2.1251038229964 +- eps
    atan2(-10.5, -6.9) shouldBe -2.1521765105998 +- eps
    atan2(-10.9, 0.0) shouldBe -1.5707963267949 +- eps
    atan2(-10.9, 6.0) shouldBe -1.0676010032055 +- eps
    atan2(-10.9, 6.1) shouldBe -1.0605874125266 +- eps
    atan2(-10.9, 6.5) shouldBe -1.0330795226463 +- eps
    atan2(-10.9, 6.9) shouldBe -1.0064452260725 +- eps
    atan2(-10.9, -6.0) shouldBe -2.0739916503843 +- eps
    atan2(-10.9, -6.1) shouldBe -2.0810052410632 +- eps
    atan2(-10.9, -6.5) shouldBe -2.1085131309435 +- eps
    atan2(-10.9, -6.9) shouldBe -2.1351474275173 +- eps
  }

  "Double.hypot" should work in {
    hypot(0.0, 0.0) shouldBe 0.0 +- eps
    hypot(0.0, 6.0) shouldBe 6.0 +- eps
    hypot(0.0, 6.1) shouldBe 6.1 +- eps
    hypot(0.0, 6.5) shouldBe 6.5 +- eps
    hypot(0.0, 6.9) shouldBe 6.9 +- eps
    hypot(0.0, -6.0) shouldBe 6.0 +- eps
    hypot(0.0, -6.1) shouldBe 6.1 +- eps
    hypot(0.0, -6.5) shouldBe 6.5 +- eps
    hypot(0.0, -6.9) shouldBe 6.9 +- eps
    hypot(10.0, 0.0) shouldBe 10.0 +- eps
    hypot(10.0, 6.0) shouldBe 11.661903789691 +- eps
    hypot(10.0, 6.1) shouldBe 11.71366723106 +- eps
    hypot(10.0, 6.5) shouldBe 11.926860441877 +- eps
    hypot(10.0, 6.9) shouldBe 12.149485585818 +- eps
    hypot(10.0, -6.0) shouldBe 11.661903789691 +- eps
    hypot(10.0, -6.1) shouldBe 11.71366723106 +- eps
    hypot(10.0, -6.5) shouldBe 11.926860441877 +- eps
    hypot(10.0, -6.9) shouldBe 12.149485585818 +- eps
    hypot(10.1, 0.0) shouldBe 10.1 +- eps
    hypot(10.1, 6.0) shouldBe 11.747765745026 +- eps
    hypot(10.1, 6.1) shouldBe 11.799152511939 +- eps
    hypot(10.1, 6.5) shouldBe 12.010828447697 +- eps
    hypot(10.1, 6.9) shouldBe 12.231925441238 +- eps
    hypot(10.1, -6.0) shouldBe 11.747765745026 +- eps
    hypot(10.1, -6.1) shouldBe 11.799152511939 +- eps
    hypot(10.1, -6.5) shouldBe 12.010828447697 +- eps
    hypot(10.1, -6.9) shouldBe 12.231925441238 +- eps
    hypot(10.5, 0.0) shouldBe 10.5 +- eps
    hypot(10.5, 6.0) shouldBe 12.093386622448 +- eps
    hypot(10.5, 6.1) shouldBe 12.143310915891 +- eps
    hypot(10.5, 6.5) shouldBe 12.349089035228 +- eps
    hypot(10.5, 6.9) shouldBe 12.564234954823 +- eps
    hypot(10.5, -6.0) shouldBe 12.093386622448 +- eps
    hypot(10.5, -6.1) shouldBe 12.143310915891 +- eps
    hypot(10.5, -6.5) shouldBe 12.349089035228 +- eps
    hypot(10.5, -6.9) shouldBe 12.564234954823 +- eps
    hypot(10.9, 0.0) shouldBe 10.9 +- eps
    hypot(10.9, 6.0) shouldBe 12.442266674525 +- eps
    hypot(10.9, 6.1) shouldBe 12.490796611906 +- eps
    hypot(10.9, 6.5) shouldBe 12.69094165143 +- eps
    hypot(10.9, 6.9) shouldBe 12.900387591076 +- eps
    hypot(10.9, -6.0) shouldBe 12.442266674525 +- eps
    hypot(10.9, -6.1) shouldBe 12.490796611906 +- eps
    hypot(10.9, -6.5) shouldBe 12.69094165143 +- eps
    hypot(10.9, -6.9) shouldBe 12.900387591076 +- eps
    hypot(-10.0, 0.0) shouldBe 10.0 +- eps
    hypot(-10.0, 6.0) shouldBe 11.661903789691 +- eps
    hypot(-10.0, 6.1) shouldBe 11.71366723106 +- eps
    hypot(-10.0, 6.5) shouldBe 11.926860441877 +- eps
    hypot(-10.0, 6.9) shouldBe 12.149485585818 +- eps
    hypot(-10.0, -6.0) shouldBe 11.661903789691 +- eps
    hypot(-10.0, -6.1) shouldBe 11.71366723106 +- eps
    hypot(-10.0, -6.5) shouldBe 11.926860441877 +- eps
    hypot(-10.0, -6.9) shouldBe 12.149485585818 +- eps
    hypot(-10.1, 0.0) shouldBe 10.1 +- eps
    hypot(-10.1, 6.0) shouldBe 11.747765745026 +- eps
    hypot(-10.1, 6.1) shouldBe 11.799152511939 +- eps
    hypot(-10.1, 6.5) shouldBe 12.010828447697 +- eps
    hypot(-10.1, 6.9) shouldBe 12.231925441238 +- eps
    hypot(-10.1, -6.0) shouldBe 11.747765745026 +- eps
    hypot(-10.1, -6.1) shouldBe 11.799152511939 +- eps
    hypot(-10.1, -6.5) shouldBe 12.010828447697 +- eps
    hypot(-10.1, -6.9) shouldBe 12.231925441238 +- eps
    hypot(-10.5, 0.0) shouldBe 10.5 +- eps
    hypot(-10.5, 6.0) shouldBe 12.093386622448 +- eps
    hypot(-10.5, 6.1) shouldBe 12.143310915891 +- eps
    hypot(-10.5, 6.5) shouldBe 12.349089035228 +- eps
    hypot(-10.5, 6.9) shouldBe 12.564234954823 +- eps
    hypot(-10.5, -6.0) shouldBe 12.093386622448 +- eps
    hypot(-10.5, -6.1) shouldBe 12.143310915891 +- eps
    hypot(-10.5, -6.5) shouldBe 12.349089035228 +- eps
    hypot(-10.5, -6.9) shouldBe 12.564234954823 +- eps
    hypot(-10.9, 0.0) shouldBe 10.9 +- eps
    hypot(-10.9, 6.0) shouldBe 12.442266674525 +- eps
    hypot(-10.9, 6.1) shouldBe 12.490796611906 +- eps
    hypot(-10.9, 6.5) shouldBe 12.69094165143 +- eps
    hypot(-10.9, 6.9) shouldBe 12.900387591076 +- eps
    hypot(-10.9, -6.0) shouldBe 12.442266674525 +- eps
    hypot(-10.9, -6.1) shouldBe 12.490796611906 +- eps
    hypot(-10.9, -6.5) shouldBe 12.69094165143 +- eps
    hypot(-10.9, -6.9) shouldBe 12.900387591076 +- eps
  }

  "Double.hypotApx" should work in {
    hypotApx(0.0, 0.0) shouldBe 0.0 +- eps
    hypotApx(0.0, 6.0) shouldBe 6.0 +- eps
    hypotApx(0.0, 6.1) shouldBe 6.1 +- eps
    hypotApx(0.0, 6.5) shouldBe 6.5 +- eps
    hypotApx(0.0, 6.9) shouldBe 6.9 +- eps
    hypotApx(0.0, -6.0) shouldBe 6.0 +- eps
    hypotApx(0.0, -6.1) shouldBe 6.1 +- eps
    hypotApx(0.0, -6.5) shouldBe 6.5 +- eps
    hypotApx(0.0, -6.9) shouldBe 6.9 +- eps
    hypotApx(10.0, 0.0) shouldBe 10.0 +- eps
    hypotApx(10.0, 6.0) shouldBe 13.514718592167 +- eps
    hypotApx(10.0, 6.1) shouldBe 13.57329723537 +- eps
    hypotApx(10.0, 6.5) shouldBe 13.807611808181 +- eps
    hypotApx(10.0, 6.9) shouldBe 14.041926380992 +- eps
    hypotApx(10.0, -6.0) shouldBe 13.514718592167 +- eps
    hypotApx(10.0, -6.1) shouldBe 13.57329723537 +- eps
    hypotApx(10.0, -6.5) shouldBe 13.807611808181 +- eps
    hypotApx(10.0, -6.9) shouldBe 14.041926380992 +- eps
    hypotApx(10.1, 0.0) shouldBe 10.1 +- eps
    hypotApx(10.1, 6.0) shouldBe 13.614718592167 +- eps
    hypotApx(10.1, 6.1) shouldBe 13.67329723537 +- eps
    hypotApx(10.1, 6.5) shouldBe 13.907611808181 +- eps
    hypotApx(10.1, 6.9) shouldBe 14.141926380992 +- eps
    hypotApx(10.1, -6.0) shouldBe 13.614718592167 +- eps
    hypotApx(10.1, -6.1) shouldBe 13.67329723537 +- eps
    hypotApx(10.1, -6.5) shouldBe 13.907611808181 +- eps
    hypotApx(10.1, -6.9) shouldBe 14.141926380992 +- eps
    hypotApx(10.5, 0.0) shouldBe 10.5 +- eps
    hypotApx(10.5, 6.0) shouldBe 14.014718592167 +- eps
    hypotApx(10.5, 6.1) shouldBe 14.07329723537 +- eps
    hypotApx(10.5, 6.5) shouldBe 14.307611808181 +- eps
    hypotApx(10.5, 6.9) shouldBe 14.541926380992 +- eps
    hypotApx(10.5, -6.0) shouldBe 14.014718592167 +- eps
    hypotApx(10.5, -6.1) shouldBe 14.07329723537 +- eps
    hypotApx(10.5, -6.5) shouldBe 14.307611808181 +- eps
    hypotApx(10.5, -6.9) shouldBe 14.541926380992 +- eps
    hypotApx(10.9, 0.0) shouldBe 10.9 +- eps
    hypotApx(10.9, 6.0) shouldBe 14.414718592167 +- eps
    hypotApx(10.9, 6.1) shouldBe 14.47329723537 +- eps
    hypotApx(10.9, 6.5) shouldBe 14.707611808181 +- eps
    hypotApx(10.9, 6.9) shouldBe 14.941926380992 +- eps
    hypotApx(10.9, -6.0) shouldBe 14.414718592167 +- eps
    hypotApx(10.9, -6.1) shouldBe 14.47329723537 +- eps
    hypotApx(10.9, -6.5) shouldBe 14.707611808181 +- eps
    hypotApx(10.9, -6.9) shouldBe 14.941926380992 +- eps
    hypotApx(-10.0, 0.0) shouldBe 10.0 +- eps
    hypotApx(-10.0, 6.0) shouldBe 13.514718592167 +- eps
    hypotApx(-10.0, 6.1) shouldBe 13.57329723537 +- eps
    hypotApx(-10.0, 6.5) shouldBe 13.807611808181 +- eps
    hypotApx(-10.0, 6.9) shouldBe 14.041926380992 +- eps
    hypotApx(-10.0, -6.0) shouldBe 13.514718592167 +- eps
    hypotApx(-10.0, -6.1) shouldBe 13.57329723537 +- eps
    hypotApx(-10.0, -6.5) shouldBe 13.807611808181 +- eps
    hypotApx(-10.0, -6.9) shouldBe 14.041926380992 +- eps
    hypotApx(-10.1, 0.0) shouldBe 10.1 +- eps
    hypotApx(-10.1, 6.0) shouldBe 13.614718592167 +- eps
    hypotApx(-10.1, 6.1) shouldBe 13.67329723537 +- eps
    hypotApx(-10.1, 6.5) shouldBe 13.907611808181 +- eps
    hypotApx(-10.1, 6.9) shouldBe 14.141926380992 +- eps
    hypotApx(-10.1, -6.0) shouldBe 13.614718592167 +- eps
    hypotApx(-10.1, -6.1) shouldBe 13.67329723537 +- eps
    hypotApx(-10.1, -6.5) shouldBe 13.907611808181 +- eps
    hypotApx(-10.1, -6.9) shouldBe 14.141926380992 +- eps
    hypotApx(-10.5, 0.0) shouldBe 10.5 +- eps
    hypotApx(-10.5, 6.0) shouldBe 14.014718592167 +- eps
    hypotApx(-10.5, 6.1) shouldBe 14.07329723537 +- eps
    hypotApx(-10.5, 6.5) shouldBe 14.307611808181 +- eps
    hypotApx(-10.5, 6.9) shouldBe 14.541926380992 +- eps
    hypotApx(-10.5, -6.0) shouldBe 14.014718592167 +- eps
    hypotApx(-10.5, -6.1) shouldBe 14.07329723537 +- eps
    hypotApx(-10.5, -6.5) shouldBe 14.307611808181 +- eps
    hypotApx(-10.5, -6.9) shouldBe 14.541926380992 +- eps
    hypotApx(-10.9, 0.0) shouldBe 10.9 +- eps
    hypotApx(-10.9, 6.0) shouldBe 14.414718592167 +- eps
    hypotApx(-10.9, 6.1) shouldBe 14.47329723537 +- eps
    hypotApx(-10.9, 6.5) shouldBe 14.707611808181 +- eps
    hypotApx(-10.9, 6.9) shouldBe 14.941926380992 +- eps
    hypotApx(-10.9, -6.0) shouldBe 14.414718592167 +- eps
    hypotApx(-10.9, -6.1) shouldBe 14.47329723537 +- eps
    hypotApx(-10.9, -6.5) shouldBe 14.707611808181 +- eps
    hypotApx(-10.9, -6.9) shouldBe 14.941926380992 +- eps
  }

  "Double.pow" should work in {
    pow(0.0, 0.0) shouldBe 1.0 +- eps
    pow(0.0, 6.0) shouldBe 0.0 +- eps
    pow(0.0, 6.1) shouldBe 0.0 +- eps
    pow(0.0, 6.5) shouldBe 0.0 +- eps
    pow(0.0, 6.9) shouldBe 0.0 +- eps
    pow(0.0, -6.0) shouldBe inf +- eps
    pow(0.0, -6.1) shouldBe inf +- eps
    pow(0.0, -6.5) shouldBe inf +- eps
    pow(0.0, -6.9) shouldBe inf +- eps
    pow(10.0, 0.0) shouldBe 1.0 +- eps
    pow(10.0, 6.0) shouldBe 1000000.0 +- eps
    pow(10.0, 6.1) shouldBe 1258925.4117942 +- eps
    pow(10.0, 6.5) shouldBe 3162277.6601684 +- eps
    pow(10.0, 6.9) shouldBe 7943282.3472428 +- eps
    pow(10.0, -6.0) shouldBe 1e-06 +- eps
    pow(10.0, -6.1) shouldBe 7.9432823472428e-07 +- eps
    pow(10.0, -6.5) shouldBe 3.1622776601684e-07 +- eps
    pow(10.0, -6.9) shouldBe 1.2589254117942e-07 +- eps
    pow(10.1, 0.0) shouldBe 1.0 +- eps
    pow(10.1, 6.0) shouldBe 1061520.150601 +- eps
    pow(10.1, 6.1) shouldBe 1337705.0915425 +- eps
    pow(10.1, 6.5) shouldBe 3373563.8135854 +- eps
    pow(10.1, 6.9) shouldBe 8507804.0565802 +- eps
    pow(10.1, -6.0) shouldBe 9.4204523525421e-07 +- eps
    pow(10.1, -6.1) shouldBe 7.4754892264552e-07 +- eps
    pow(10.1, -6.5) shouldBe 2.9642243492564e-07 +- eps
    pow(10.1, -6.9) shouldBe 1.1753914327947e-07 +- eps
    pow(10.5, 0.0) shouldBe 1.0 +- eps
    pow(10.5, 6.0) shouldBe 1340095.640625 +- eps
    pow(10.5, 6.1) shouldBe 1695331.8624647 +- eps
    pow(10.5, 6.5) shouldBe 4342406.1789787 +- eps
    pow(10.5, 6.9) shouldBe 11122596.018351 +- eps
    pow(10.5, -6.0) shouldBe 7.4621539663663e-07 +- eps
    pow(10.5, -6.1) shouldBe 5.8985501431336e-07 +- eps
    pow(10.5, -6.5) shouldBe 2.3028707098865e-07 +- eps
    pow(10.5, -6.9) shouldBe 8.9907068309427e-08 +- eps
    pow(10.9, 0.0) shouldBe 1.0 +- eps
    pow(10.9, 6.0) shouldBe 1677100.110841 +- eps
    pow(10.9, 6.1) shouldBe 2129617.6495827 +- eps
    pow(10.9, 6.5) shouldBe 5536970.8434697 +- eps
    pow(10.9, 6.9) shouldBe 14396033.07544 +- eps
    pow(10.9, -6.0) shouldBe 5.9626732687922e-07 +- eps
    pow(10.9, -6.1) shouldBe 4.695678589046e-07 +- eps
    pow(10.9, -6.5) shouldBe 1.8060416575597e-07 +- eps
    pow(10.9, -6.9) shouldBe 6.9463580332142e-08 +- eps
    pow(-10.0, 0.0) shouldBe 1.0 +- eps
    pow(-10.0, 6.0) shouldBe 1000000.0 +- eps
    pow(-10.0, -6.0) shouldBe 1e-06 +- eps
    pow(-10.1, 0.0) shouldBe 1.0 +- eps
    pow(-10.1, 6.0) shouldBe 1061520.150601 +- eps
    pow(-10.1, -6.0) shouldBe 9.4204523525421e-07 +- eps
    pow(-10.5, 0.0) shouldBe 1.0 +- eps
    pow(-10.5, 6.0) shouldBe 1340095.640625 +- eps
    pow(-10.5, -6.0) shouldBe 7.4621539663663e-07 +- eps
    pow(-10.9, 0.0) shouldBe 1.0 +- eps
    pow(-10.9, 6.0) shouldBe 1677100.110841 +- eps
    pow(-10.9, -6.0) shouldBe 5.9626732687922e-07 +- eps
  }

  "Double.difsqr" should work in {
    difSqr(0.0, 0.0) shouldBe 0.0 +- eps
    difSqr(0.0, 6.0) shouldBe -36.0 +- eps
    difSqr(0.0, 6.1) shouldBe -37.21 +- eps
    difSqr(0.0, 6.5) shouldBe -42.25 +- eps
    difSqr(0.0, 6.9) shouldBe -47.61 +- eps
    difSqr(0.0, -6.0) shouldBe -36.0 +- eps
    difSqr(0.0, -6.1) shouldBe -37.21 +- eps
    difSqr(0.0, -6.5) shouldBe -42.25 +- eps
    difSqr(0.0, -6.9) shouldBe -47.61 +- eps
    difSqr(10.0, 0.0) shouldBe 100.0 +- eps
    difSqr(10.0, 6.0) shouldBe 64.0 +- eps
    difSqr(10.0, 6.1) shouldBe 62.79 +- eps
    difSqr(10.0, 6.5) shouldBe 57.75 +- eps
    difSqr(10.0, 6.9) shouldBe 52.39 +- eps
    difSqr(10.0, -6.0) shouldBe 64.0 +- eps
    difSqr(10.0, -6.1) shouldBe 62.79 +- eps
    difSqr(10.0, -6.5) shouldBe 57.75 +- eps
    difSqr(10.0, -6.9) shouldBe 52.39 +- eps
    difSqr(10.1, 0.0) shouldBe 102.01 +- eps
    difSqr(10.1, 6.0) shouldBe 66.01 +- eps
    difSqr(10.1, 6.1) shouldBe 64.8 +- eps
    difSqr(10.1, 6.5) shouldBe 59.76 +- eps
    difSqr(10.1, 6.9) shouldBe 54.4 +- eps
    difSqr(10.1, -6.0) shouldBe 66.01 +- eps
    difSqr(10.1, -6.1) shouldBe 64.8 +- eps
    difSqr(10.1, -6.5) shouldBe 59.76 +- eps
    difSqr(10.1, -6.9) shouldBe 54.4 +- eps
    difSqr(10.5, 0.0) shouldBe 110.25 +- eps
    difSqr(10.5, 6.0) shouldBe 74.25 +- eps
    difSqr(10.5, 6.1) shouldBe 73.04 +- eps
    difSqr(10.5, 6.5) shouldBe 68.0 +- eps
    difSqr(10.5, 6.9) shouldBe 62.64 +- eps
    difSqr(10.5, -6.0) shouldBe 74.25 +- eps
    difSqr(10.5, -6.1) shouldBe 73.04 +- eps
    difSqr(10.5, -6.5) shouldBe 68.0 +- eps
    difSqr(10.5, -6.9) shouldBe 62.64 +- eps
    difSqr(10.9, 0.0) shouldBe 118.81 +- eps
    difSqr(10.9, 6.0) shouldBe 82.81 +- eps
    difSqr(10.9, 6.1) shouldBe 81.6 +- eps
    difSqr(10.9, 6.5) shouldBe 76.56 +- eps
    difSqr(10.9, 6.9) shouldBe 71.2 +- eps
    difSqr(10.9, -6.0) shouldBe 82.81 +- eps
    difSqr(10.9, -6.1) shouldBe 81.6 +- eps
    difSqr(10.9, -6.5) shouldBe 76.56 +- eps
    difSqr(10.9, -6.9) shouldBe 71.2 +- eps
    difSqr(-10.0, 0.0) shouldBe 100.0 +- eps
    difSqr(-10.0, 6.0) shouldBe 64.0 +- eps
    difSqr(-10.0, 6.1) shouldBe 62.79 +- eps
    difSqr(-10.0, 6.5) shouldBe 57.75 +- eps
    difSqr(-10.0, 6.9) shouldBe 52.39 +- eps
    difSqr(-10.0, -6.0) shouldBe 64.0 +- eps
    difSqr(-10.0, -6.1) shouldBe 62.79 +- eps
    difSqr(-10.0, -6.5) shouldBe 57.75 +- eps
    difSqr(-10.0, -6.9) shouldBe 52.39 +- eps
    difSqr(-10.1, 0.0) shouldBe 102.01 +- eps
    difSqr(-10.1, 6.0) shouldBe 66.01 +- eps
    difSqr(-10.1, 6.1) shouldBe 64.8 +- eps
    difSqr(-10.1, 6.5) shouldBe 59.76 +- eps
    difSqr(-10.1, 6.9) shouldBe 54.4 +- eps
    difSqr(-10.1, -6.0) shouldBe 66.01 +- eps
    difSqr(-10.1, -6.1) shouldBe 64.8 +- eps
    difSqr(-10.1, -6.5) shouldBe 59.76 +- eps
    difSqr(-10.1, -6.9) shouldBe 54.4 +- eps
    difSqr(-10.5, 0.0) shouldBe 110.25 +- eps
    difSqr(-10.5, 6.0) shouldBe 74.25 +- eps
    difSqr(-10.5, 6.1) shouldBe 73.04 +- eps
    difSqr(-10.5, 6.5) shouldBe 68.0 +- eps
    difSqr(-10.5, 6.9) shouldBe 62.64 +- eps
    difSqr(-10.5, -6.0) shouldBe 74.25 +- eps
    difSqr(-10.5, -6.1) shouldBe 73.04 +- eps
    difSqr(-10.5, -6.5) shouldBe 68.0 +- eps
    difSqr(-10.5, -6.9) shouldBe 62.64 +- eps
    difSqr(-10.9, 0.0) shouldBe 118.81 +- eps
    difSqr(-10.9, 6.0) shouldBe 82.81 +- eps
    difSqr(-10.9, 6.1) shouldBe 81.6 +- eps
    difSqr(-10.9, 6.5) shouldBe 76.56 +- eps
    difSqr(-10.9, 6.9) shouldBe 71.2 +- eps
    difSqr(-10.9, -6.0) shouldBe 82.81 +- eps
    difSqr(-10.9, -6.1) shouldBe 81.6 +- eps
    difSqr(-10.9, -6.5) shouldBe 76.56 +- eps
    difSqr(-10.9, -6.9) shouldBe 71.2 +- eps
  }

  "Double.sumsqr" should work in {
    sumSqr(0.0, 0.0) shouldBe 0.0 +- eps
    sumSqr(0.0, 6.0) shouldBe 36.0 +- eps
    sumSqr(0.0, 6.1) shouldBe 37.21 +- eps
    sumSqr(0.0, 6.5) shouldBe 42.25 +- eps
    sumSqr(0.0, 6.9) shouldBe 47.61 +- eps
    sumSqr(0.0, -6.0) shouldBe 36.0 +- eps
    sumSqr(0.0, -6.1) shouldBe 37.21 +- eps
    sumSqr(0.0, -6.5) shouldBe 42.25 +- eps
    sumSqr(0.0, -6.9) shouldBe 47.61 +- eps
    sumSqr(10.0, 0.0) shouldBe 100.0 +- eps
    sumSqr(10.0, 6.0) shouldBe 136.0 +- eps
    sumSqr(10.0, 6.1) shouldBe 137.21 +- eps
    sumSqr(10.0, 6.5) shouldBe 142.25 +- eps
    sumSqr(10.0, 6.9) shouldBe 147.61 +- eps
    sumSqr(10.0, -6.0) shouldBe 136.0 +- eps
    sumSqr(10.0, -6.1) shouldBe 137.21 +- eps
    sumSqr(10.0, -6.5) shouldBe 142.25 +- eps
    sumSqr(10.0, -6.9) shouldBe 147.61 +- eps
    sumSqr(10.1, 0.0) shouldBe 102.01 +- eps
    sumSqr(10.1, 6.0) shouldBe 138.01 +- eps
    sumSqr(10.1, 6.1) shouldBe 139.22 +- eps
    sumSqr(10.1, 6.5) shouldBe 144.26 +- eps
    sumSqr(10.1, 6.9) shouldBe 149.62 +- eps
    sumSqr(10.1, -6.0) shouldBe 138.01 +- eps
    sumSqr(10.1, -6.1) shouldBe 139.22 +- eps
    sumSqr(10.1, -6.5) shouldBe 144.26 +- eps
    sumSqr(10.1, -6.9) shouldBe 149.62 +- eps
    sumSqr(10.5, 0.0) shouldBe 110.25 +- eps
    sumSqr(10.5, 6.0) shouldBe 146.25 +- eps
    sumSqr(10.5, 6.1) shouldBe 147.46 +- eps
    sumSqr(10.5, 6.5) shouldBe 152.5 +- eps
    sumSqr(10.5, 6.9) shouldBe 157.86 +- eps
    sumSqr(10.5, -6.0) shouldBe 146.25 +- eps
    sumSqr(10.5, -6.1) shouldBe 147.46 +- eps
    sumSqr(10.5, -6.5) shouldBe 152.5 +- eps
    sumSqr(10.5, -6.9) shouldBe 157.86 +- eps
    sumSqr(10.9, 0.0) shouldBe 118.81 +- eps
    sumSqr(10.9, 6.0) shouldBe 154.81 +- eps
    sumSqr(10.9, 6.1) shouldBe 156.02 +- eps
    sumSqr(10.9, 6.5) shouldBe 161.06 +- eps
    sumSqr(10.9, 6.9) shouldBe 166.42 +- eps
    sumSqr(10.9, -6.0) shouldBe 154.81 +- eps
    sumSqr(10.9, -6.1) shouldBe 156.02 +- eps
    sumSqr(10.9, -6.5) shouldBe 161.06 +- eps
    sumSqr(10.9, -6.9) shouldBe 166.42 +- eps
    sumSqr(-10.0, 0.0) shouldBe 100.0 +- eps
    sumSqr(-10.0, 6.0) shouldBe 136.0 +- eps
    sumSqr(-10.0, 6.1) shouldBe 137.21 +- eps
    sumSqr(-10.0, 6.5) shouldBe 142.25 +- eps
    sumSqr(-10.0, 6.9) shouldBe 147.61 +- eps
    sumSqr(-10.0, -6.0) shouldBe 136.0 +- eps
    sumSqr(-10.0, -6.1) shouldBe 137.21 +- eps
    sumSqr(-10.0, -6.5) shouldBe 142.25 +- eps
    sumSqr(-10.0, -6.9) shouldBe 147.61 +- eps
    sumSqr(-10.1, 0.0) shouldBe 102.01 +- eps
    sumSqr(-10.1, 6.0) shouldBe 138.01 +- eps
    sumSqr(-10.1, 6.1) shouldBe 139.22 +- eps
    sumSqr(-10.1, 6.5) shouldBe 144.26 +- eps
    sumSqr(-10.1, 6.9) shouldBe 149.62 +- eps
    sumSqr(-10.1, -6.0) shouldBe 138.01 +- eps
    sumSqr(-10.1, -6.1) shouldBe 139.22 +- eps
    sumSqr(-10.1, -6.5) shouldBe 144.26 +- eps
    sumSqr(-10.1, -6.9) shouldBe 149.62 +- eps
    sumSqr(-10.5, 0.0) shouldBe 110.25 +- eps
    sumSqr(-10.5, 6.0) shouldBe 146.25 +- eps
    sumSqr(-10.5, 6.1) shouldBe 147.46 +- eps
    sumSqr(-10.5, 6.5) shouldBe 152.5 +- eps
    sumSqr(-10.5, 6.9) shouldBe 157.86 +- eps
    sumSqr(-10.5, -6.0) shouldBe 146.25 +- eps
    sumSqr(-10.5, -6.1) shouldBe 147.46 +- eps
    sumSqr(-10.5, -6.5) shouldBe 152.5 +- eps
    sumSqr(-10.5, -6.9) shouldBe 157.86 +- eps
    sumSqr(-10.9, 0.0) shouldBe 118.81 +- eps
    sumSqr(-10.9, 6.0) shouldBe 154.81 +- eps
    sumSqr(-10.9, 6.1) shouldBe 156.02 +- eps
    sumSqr(-10.9, 6.5) shouldBe 161.06 +- eps
    sumSqr(-10.9, 6.9) shouldBe 166.42 +- eps
    sumSqr(-10.9, -6.0) shouldBe 154.81 +- eps
    sumSqr(-10.9, -6.1) shouldBe 156.02 +- eps
    sumSqr(-10.9, -6.5) shouldBe 161.06 +- eps
    sumSqr(-10.9, -6.9) shouldBe 166.42 +- eps
  }

  "Double.sqrsum" should work in {
    sqrSum(0.0, 0.0) shouldBe 0.0 +- eps
    sqrSum(0.0, 6.0) shouldBe 36.0 +- eps
    sqrSum(0.0, 6.1) shouldBe 37.21 +- eps
    sqrSum(0.0, 6.5) shouldBe 42.25 +- eps
    sqrSum(0.0, 6.9) shouldBe 47.61 +- eps
    sqrSum(0.0, -6.0) shouldBe 36.0 +- eps
    sqrSum(0.0, -6.1) shouldBe 37.21 +- eps
    sqrSum(0.0, -6.5) shouldBe 42.25 +- eps
    sqrSum(0.0, -6.9) shouldBe 47.61 +- eps
    sqrSum(10.0, 0.0) shouldBe 100.0 +- eps
    sqrSum(10.0, 6.0) shouldBe 256.0 +- eps
    sqrSum(10.0, 6.1) shouldBe 259.21 +- eps
    sqrSum(10.0, 6.5) shouldBe 272.25 +- eps
    sqrSum(10.0, 6.9) shouldBe 285.61 +- eps
    sqrSum(10.0, -6.0) shouldBe 16.0 +- eps
    sqrSum(10.0, -6.1) shouldBe 15.21 +- eps
    sqrSum(10.0, -6.5) shouldBe 12.25 +- eps
    sqrSum(10.0, -6.9) shouldBe 9.61 +- eps
    sqrSum(10.1, 0.0) shouldBe 102.01 +- eps
    sqrSum(10.1, 6.0) shouldBe 259.21 +- eps
    sqrSum(10.1, 6.1) shouldBe 262.44 +- eps
    sqrSum(10.1, 6.5) shouldBe 275.56 +- eps
    sqrSum(10.1, 6.9) shouldBe 289.0 +- eps
    sqrSum(10.1, -6.0) shouldBe 16.81 +- eps
    sqrSum(10.1, -6.1) shouldBe 16.0 +- eps
    sqrSum(10.1, -6.5) shouldBe 12.96 +- eps
    sqrSum(10.1, -6.9) shouldBe 10.24 +- eps
    sqrSum(10.5, 0.0) shouldBe 110.25 +- eps
    sqrSum(10.5, 6.0) shouldBe 272.25 +- eps
    sqrSum(10.5, 6.1) shouldBe 275.56 +- eps
    sqrSum(10.5, 6.5) shouldBe 289.0 +- eps
    sqrSum(10.5, 6.9) shouldBe 302.76 +- eps
    sqrSum(10.5, -6.0) shouldBe 20.25 +- eps
    sqrSum(10.5, -6.1) shouldBe 19.36 +- eps
    sqrSum(10.5, -6.5) shouldBe 16.0 +- eps
    sqrSum(10.5, -6.9) shouldBe 12.96 +- eps
    sqrSum(10.9, 0.0) shouldBe 118.81 +- eps
    sqrSum(10.9, 6.0) shouldBe 285.61 +- eps
    sqrSum(10.9, 6.1) shouldBe 289.0 +- eps
    sqrSum(10.9, 6.5) shouldBe 302.76 +- eps
    sqrSum(10.9, 6.9) shouldBe 316.84 +- eps
    sqrSum(10.9, -6.0) shouldBe 24.01 +- eps
    sqrSum(10.9, -6.1) shouldBe 23.04 +- eps
    sqrSum(10.9, -6.5) shouldBe 19.36 +- eps
    sqrSum(10.9, -6.9) shouldBe 16.0 +- eps
    sqrSum(-10.0, 0.0) shouldBe 100.0 +- eps
    sqrSum(-10.0, 6.0) shouldBe 16.0 +- eps
    sqrSum(-10.0, 6.1) shouldBe 15.21 +- eps
    sqrSum(-10.0, 6.5) shouldBe 12.25 +- eps
    sqrSum(-10.0, 6.9) shouldBe 9.61 +- eps
    sqrSum(-10.0, -6.0) shouldBe 256.0 +- eps
    sqrSum(-10.0, -6.1) shouldBe 259.21 +- eps
    sqrSum(-10.0, -6.5) shouldBe 272.25 +- eps
    sqrSum(-10.0, -6.9) shouldBe 285.61 +- eps
    sqrSum(-10.1, 0.0) shouldBe 102.01 +- eps
    sqrSum(-10.1, 6.0) shouldBe 16.81 +- eps
    sqrSum(-10.1, 6.1) shouldBe 16.0 +- eps
    sqrSum(-10.1, 6.5) shouldBe 12.96 +- eps
    sqrSum(-10.1, 6.9) shouldBe 10.24 +- eps
    sqrSum(-10.1, -6.0) shouldBe 259.21 +- eps
    sqrSum(-10.1, -6.1) shouldBe 262.44 +- eps
    sqrSum(-10.1, -6.5) shouldBe 275.56 +- eps
    sqrSum(-10.1, -6.9) shouldBe 289.0 +- eps
    sqrSum(-10.5, 0.0) shouldBe 110.25 +- eps
    sqrSum(-10.5, 6.0) shouldBe 20.25 +- eps
    sqrSum(-10.5, 6.1) shouldBe 19.36 +- eps
    sqrSum(-10.5, 6.5) shouldBe 16.0 +- eps
    sqrSum(-10.5, 6.9) shouldBe 12.96 +- eps
    sqrSum(-10.5, -6.0) shouldBe 272.25 +- eps
    sqrSum(-10.5, -6.1) shouldBe 275.56 +- eps
    sqrSum(-10.5, -6.5) shouldBe 289.0 +- eps
    sqrSum(-10.5, -6.9) shouldBe 302.76 +- eps
    sqrSum(-10.9, 0.0) shouldBe 118.81 +- eps
    sqrSum(-10.9, 6.0) shouldBe 24.01 +- eps
    sqrSum(-10.9, 6.1) shouldBe 23.04 +- eps
    sqrSum(-10.9, 6.5) shouldBe 19.36 +- eps
    sqrSum(-10.9, 6.9) shouldBe 16.0 +- eps
    sqrSum(-10.9, -6.0) shouldBe 285.61 +- eps
    sqrSum(-10.9, -6.1) shouldBe 289.0 +- eps
    sqrSum(-10.9, -6.5) shouldBe 302.76 +- eps
    sqrSum(-10.9, -6.9) shouldBe 316.84 +- eps
  }

  "Double.sqrdif" should work in {
    sqrDif(0.0, 0.0) shouldBe 0.0 +- eps
    sqrDif(0.0, 6.0) shouldBe 36.0 +- eps
    sqrDif(0.0, 6.1) shouldBe 37.21 +- eps
    sqrDif(0.0, 6.5) shouldBe 42.25 +- eps
    sqrDif(0.0, 6.9) shouldBe 47.61 +- eps
    sqrDif(0.0, -6.0) shouldBe 36.0 +- eps
    sqrDif(0.0, -6.1) shouldBe 37.21 +- eps
    sqrDif(0.0, -6.5) shouldBe 42.25 +- eps
    sqrDif(0.0, -6.9) shouldBe 47.61 +- eps
    sqrDif(10.0, 0.0) shouldBe 100.0 +- eps
    sqrDif(10.0, 6.0) shouldBe 16.0 +- eps
    sqrDif(10.0, 6.1) shouldBe 15.21 +- eps
    sqrDif(10.0, 6.5) shouldBe 12.25 +- eps
    sqrDif(10.0, 6.9) shouldBe 9.61 +- eps
    sqrDif(10.0, -6.0) shouldBe 256.0 +- eps
    sqrDif(10.0, -6.1) shouldBe 259.21 +- eps
    sqrDif(10.0, -6.5) shouldBe 272.25 +- eps
    sqrDif(10.0, -6.9) shouldBe 285.61 +- eps
    sqrDif(10.1, 0.0) shouldBe 102.01 +- eps
    sqrDif(10.1, 6.0) shouldBe 16.81 +- eps
    sqrDif(10.1, 6.1) shouldBe 16.0 +- eps
    sqrDif(10.1, 6.5) shouldBe 12.96 +- eps
    sqrDif(10.1, 6.9) shouldBe 10.24 +- eps
    sqrDif(10.1, -6.0) shouldBe 259.21 +- eps
    sqrDif(10.1, -6.1) shouldBe 262.44 +- eps
    sqrDif(10.1, -6.5) shouldBe 275.56 +- eps
    sqrDif(10.1, -6.9) shouldBe 289.0 +- eps
    sqrDif(10.5, 0.0) shouldBe 110.25 +- eps
    sqrDif(10.5, 6.0) shouldBe 20.25 +- eps
    sqrDif(10.5, 6.1) shouldBe 19.36 +- eps
    sqrDif(10.5, 6.5) shouldBe 16.0 +- eps
    sqrDif(10.5, 6.9) shouldBe 12.96 +- eps
    sqrDif(10.5, -6.0) shouldBe 272.25 +- eps
    sqrDif(10.5, -6.1) shouldBe 275.56 +- eps
    sqrDif(10.5, -6.5) shouldBe 289.0 +- eps
    sqrDif(10.5, -6.9) shouldBe 302.76 +- eps
    sqrDif(10.9, 0.0) shouldBe 118.81 +- eps
    sqrDif(10.9, 6.0) shouldBe 24.01 +- eps
    sqrDif(10.9, 6.1) shouldBe 23.04 +- eps
    sqrDif(10.9, 6.5) shouldBe 19.36 +- eps
    sqrDif(10.9, 6.9) shouldBe 16.0 +- eps
    sqrDif(10.9, -6.0) shouldBe 285.61 +- eps
    sqrDif(10.9, -6.1) shouldBe 289.0 +- eps
    sqrDif(10.9, -6.5) shouldBe 302.76 +- eps
    sqrDif(10.9, -6.9) shouldBe 316.84 +- eps
    sqrDif(-10.0, 0.0) shouldBe 100.0 +- eps
    sqrDif(-10.0, 6.0) shouldBe 256.0 +- eps
    sqrDif(-10.0, 6.1) shouldBe 259.21 +- eps
    sqrDif(-10.0, 6.5) shouldBe 272.25 +- eps
    sqrDif(-10.0, 6.9) shouldBe 285.61 +- eps
    sqrDif(-10.0, -6.0) shouldBe 16.0 +- eps
    sqrDif(-10.0, -6.1) shouldBe 15.21 +- eps
    sqrDif(-10.0, -6.5) shouldBe 12.25 +- eps
    sqrDif(-10.0, -6.9) shouldBe 9.61 +- eps
    sqrDif(-10.1, 0.0) shouldBe 102.01 +- eps
    sqrDif(-10.1, 6.0) shouldBe 259.21 +- eps
    sqrDif(-10.1, 6.1) shouldBe 262.44 +- eps
    sqrDif(-10.1, 6.5) shouldBe 275.56 +- eps
    sqrDif(-10.1, 6.9) shouldBe 289.0 +- eps
    sqrDif(-10.1, -6.0) shouldBe 16.81 +- eps
    sqrDif(-10.1, -6.1) shouldBe 16.0 +- eps
    sqrDif(-10.1, -6.5) shouldBe 12.96 +- eps
    sqrDif(-10.1, -6.9) shouldBe 10.24 +- eps
    sqrDif(-10.5, 0.0) shouldBe 110.25 +- eps
    sqrDif(-10.5, 6.0) shouldBe 272.25 +- eps
    sqrDif(-10.5, 6.1) shouldBe 275.56 +- eps
    sqrDif(-10.5, 6.5) shouldBe 289.0 +- eps
    sqrDif(-10.5, 6.9) shouldBe 302.76 +- eps
    sqrDif(-10.5, -6.0) shouldBe 20.25 +- eps
    sqrDif(-10.5, -6.1) shouldBe 19.36 +- eps
    sqrDif(-10.5, -6.5) shouldBe 16.0 +- eps
    sqrDif(-10.5, -6.9) shouldBe 12.96 +- eps
    sqrDif(-10.9, 0.0) shouldBe 118.81 +- eps
    sqrDif(-10.9, 6.0) shouldBe 285.61 +- eps
    sqrDif(-10.9, 6.1) shouldBe 289.0 +- eps
    sqrDif(-10.9, 6.5) shouldBe 302.76 +- eps
    sqrDif(-10.9, 6.9) shouldBe 316.84 +- eps
    sqrDif(-10.9, -6.0) shouldBe 24.01 +- eps
    sqrDif(-10.9, -6.1) shouldBe 23.04 +- eps
    sqrDif(-10.9, -6.5) shouldBe 19.36 +- eps
    sqrDif(-10.9, -6.9) shouldBe 16.0 +- eps
  }

  "Double.absdif" should work in {
    absDif(0.0, 0.0) shouldBe 0.0 +- eps
    absDif(0.0, 6.0) shouldBe 6.0 +- eps
    absDif(0.0, 6.1) shouldBe 6.1 +- eps
    absDif(0.0, 6.5) shouldBe 6.5 +- eps
    absDif(0.0, 6.9) shouldBe 6.9 +- eps
    absDif(0.0, -6.0) shouldBe 6.0 +- eps
    absDif(0.0, -6.1) shouldBe 6.1 +- eps
    absDif(0.0, -6.5) shouldBe 6.5 +- eps
    absDif(0.0, -6.9) shouldBe 6.9 +- eps
    absDif(10.0, 0.0) shouldBe 10.0 +- eps
    absDif(10.0, 6.0) shouldBe 4.0 +- eps
    absDif(10.0, 6.1) shouldBe 3.9 +- eps
    absDif(10.0, 6.5) shouldBe 3.5 +- eps
    absDif(10.0, 6.9) shouldBe 3.1 +- eps
    absDif(10.0, -6.0) shouldBe 16.0 +- eps
    absDif(10.0, -6.1) shouldBe 16.1 +- eps
    absDif(10.0, -6.5) shouldBe 16.5 +- eps
    absDif(10.0, -6.9) shouldBe 16.9 +- eps
    absDif(10.1, 0.0) shouldBe 10.1 +- eps
    absDif(10.1, 6.0) shouldBe 4.1 +- eps
    absDif(10.1, 6.1) shouldBe 4.0 +- eps
    absDif(10.1, 6.5) shouldBe 3.6 +- eps
    absDif(10.1, 6.9) shouldBe 3.2 +- eps
    absDif(10.1, -6.0) shouldBe 16.1 +- eps
    absDif(10.1, -6.1) shouldBe 16.2 +- eps
    absDif(10.1, -6.5) shouldBe 16.6 +- eps
    absDif(10.1, -6.9) shouldBe 17.0 +- eps
    absDif(10.5, 0.0) shouldBe 10.5 +- eps
    absDif(10.5, 6.0) shouldBe 4.5 +- eps
    absDif(10.5, 6.1) shouldBe 4.4 +- eps
    absDif(10.5, 6.5) shouldBe 4.0 +- eps
    absDif(10.5, 6.9) shouldBe 3.6 +- eps
    absDif(10.5, -6.0) shouldBe 16.5 +- eps
    absDif(10.5, -6.1) shouldBe 16.6 +- eps
    absDif(10.5, -6.5) shouldBe 17.0 +- eps
    absDif(10.5, -6.9) shouldBe 17.4 +- eps
    absDif(10.9, 0.0) shouldBe 10.9 +- eps
    absDif(10.9, 6.0) shouldBe 4.9 +- eps
    absDif(10.9, 6.1) shouldBe 4.8 +- eps
    absDif(10.9, 6.5) shouldBe 4.4 +- eps
    absDif(10.9, 6.9) shouldBe 4.0 +- eps
    absDif(10.9, -6.0) shouldBe 16.9 +- eps
    absDif(10.9, -6.1) shouldBe 17.0 +- eps
    absDif(10.9, -6.5) shouldBe 17.4 +- eps
    absDif(10.9, -6.9) shouldBe 17.8 +- eps
    absDif(-10.0, 0.0) shouldBe 10.0 +- eps
    absDif(-10.0, 6.0) shouldBe 16.0 +- eps
    absDif(-10.0, 6.1) shouldBe 16.1 +- eps
    absDif(-10.0, 6.5) shouldBe 16.5 +- eps
    absDif(-10.0, 6.9) shouldBe 16.9 +- eps
    absDif(-10.0, -6.0) shouldBe 4.0 +- eps
    absDif(-10.0, -6.1) shouldBe 3.9 +- eps
    absDif(-10.0, -6.5) shouldBe 3.5 +- eps
    absDif(-10.0, -6.9) shouldBe 3.1 +- eps
    absDif(-10.1, 0.0) shouldBe 10.1 +- eps
    absDif(-10.1, 6.0) shouldBe 16.1 +- eps
    absDif(-10.1, 6.1) shouldBe 16.2 +- eps
    absDif(-10.1, 6.5) shouldBe 16.6 +- eps
    absDif(-10.1, 6.9) shouldBe 17.0 +- eps
    absDif(-10.1, -6.0) shouldBe 4.1 +- eps
    absDif(-10.1, -6.1) shouldBe 4.0 +- eps
    absDif(-10.1, -6.5) shouldBe 3.6 +- eps
    absDif(-10.1, -6.9) shouldBe 3.2 +- eps
    absDif(-10.5, 0.0) shouldBe 10.5 +- eps
    absDif(-10.5, 6.0) shouldBe 16.5 +- eps
    absDif(-10.5, 6.1) shouldBe 16.6 +- eps
    absDif(-10.5, 6.5) shouldBe 17.0 +- eps
    absDif(-10.5, 6.9) shouldBe 17.4 +- eps
    absDif(-10.5, -6.0) shouldBe 4.5 +- eps
    absDif(-10.5, -6.1) shouldBe 4.4 +- eps
    absDif(-10.5, -6.5) shouldBe 4.0 +- eps
    absDif(-10.5, -6.9) shouldBe 3.6 +- eps
    absDif(-10.9, 0.0) shouldBe 10.9 +- eps
    absDif(-10.9, 6.0) shouldBe 16.9 +- eps
    absDif(-10.9, 6.1) shouldBe 17.0 +- eps
    absDif(-10.9, 6.5) shouldBe 17.4 +- eps
    absDif(-10.9, 6.9) shouldBe 17.8 +- eps
    absDif(-10.9, -6.0) shouldBe 4.9 +- eps
    absDif(-10.9, -6.1) shouldBe 4.8 +- eps
    absDif(-10.9, -6.5) shouldBe 4.4 +- eps
    absDif(-10.9, -6.9) shouldBe 4.0 +- eps
  }

  "Double.clip2" should work in {
    clip2(0.0, 0.0) shouldBe 0.0 +- eps
    clip2(0.0, 6.0) shouldBe 0.0 +- eps
    clip2(0.0, 6.1) shouldBe 0.0 +- eps
    clip2(0.0, 6.5) shouldBe 0.0 +- eps
    clip2(0.0, 6.9) shouldBe 0.0 +- eps
    clip2(0.0, -6.0) shouldBe 6.0 +- eps
    clip2(0.0, -6.1) shouldBe 6.1 +- eps
    clip2(0.0, -6.5) shouldBe 6.5 +- eps
    clip2(0.0, -6.9) shouldBe 6.9 +- eps
    clip2(10.0, 0.0) shouldBe 0.0 +- eps
    clip2(10.0, 6.0) shouldBe 6.0 +- eps
    clip2(10.0, 6.1) shouldBe 6.1 +- eps
    clip2(10.0, 6.5) shouldBe 6.5 +- eps
    clip2(10.0, 6.9) shouldBe 6.9 +- eps
    clip2(10.0, -6.0) shouldBe 6.0 +- eps
    clip2(10.0, -6.1) shouldBe 6.1 +- eps
    clip2(10.0, -6.5) shouldBe 6.5 +- eps
    clip2(10.0, -6.9) shouldBe 6.9 +- eps
    clip2(10.1, 0.0) shouldBe 0.0 +- eps
    clip2(10.1, 6.0) shouldBe 6.0 +- eps
    clip2(10.1, 6.1) shouldBe 6.1 +- eps
    clip2(10.1, 6.5) shouldBe 6.5 +- eps
    clip2(10.1, 6.9) shouldBe 6.9 +- eps
    clip2(10.1, -6.0) shouldBe 6.0 +- eps
    clip2(10.1, -6.1) shouldBe 6.1 +- eps
    clip2(10.1, -6.5) shouldBe 6.5 +- eps
    clip2(10.1, -6.9) shouldBe 6.9 +- eps
    clip2(10.5, 0.0) shouldBe 0.0 +- eps
    clip2(10.5, 6.0) shouldBe 6.0 +- eps
    clip2(10.5, 6.1) shouldBe 6.1 +- eps
    clip2(10.5, 6.5) shouldBe 6.5 +- eps
    clip2(10.5, 6.9) shouldBe 6.9 +- eps
    clip2(10.5, -6.0) shouldBe 6.0 +- eps
    clip2(10.5, -6.1) shouldBe 6.1 +- eps
    clip2(10.5, -6.5) shouldBe 6.5 +- eps
    clip2(10.5, -6.9) shouldBe 6.9 +- eps
    clip2(10.9, 0.0) shouldBe 0.0 +- eps
    clip2(10.9, 6.0) shouldBe 6.0 +- eps
    clip2(10.9, 6.1) shouldBe 6.1 +- eps
    clip2(10.9, 6.5) shouldBe 6.5 +- eps
    clip2(10.9, 6.9) shouldBe 6.9 +- eps
    clip2(10.9, -6.0) shouldBe 6.0 +- eps
    clip2(10.9, -6.1) shouldBe 6.1 +- eps
    clip2(10.9, -6.5) shouldBe 6.5 +- eps
    clip2(10.9, -6.9) shouldBe 6.9 +- eps
    clip2(-10.0, 0.0) shouldBe -0.0 +- eps
    clip2(-10.0, 6.0) shouldBe -6.0 +- eps
    clip2(-10.0, 6.1) shouldBe -6.1 +- eps
    clip2(-10.0, 6.5) shouldBe -6.5 +- eps
    clip2(-10.0, 6.9) shouldBe -6.9 +- eps
    clip2(-10.0, -6.0) shouldBe 6.0 +- eps
    clip2(-10.0, -6.1) shouldBe 6.1 +- eps
    clip2(-10.0, -6.5) shouldBe 6.5 +- eps
    clip2(-10.0, -6.9) shouldBe 6.9 +- eps
    clip2(-10.1, 0.0) shouldBe -0.0 +- eps
    clip2(-10.1, 6.0) shouldBe -6.0 +- eps
    clip2(-10.1, 6.1) shouldBe -6.1 +- eps
    clip2(-10.1, 6.5) shouldBe -6.5 +- eps
    clip2(-10.1, 6.9) shouldBe -6.9 +- eps
    clip2(-10.1, -6.0) shouldBe 6.0 +- eps
    clip2(-10.1, -6.1) shouldBe 6.1 +- eps
    clip2(-10.1, -6.5) shouldBe 6.5 +- eps
    clip2(-10.1, -6.9) shouldBe 6.9 +- eps
    clip2(-10.5, 0.0) shouldBe -0.0 +- eps
    clip2(-10.5, 6.0) shouldBe -6.0 +- eps
    clip2(-10.5, 6.1) shouldBe -6.1 +- eps
    clip2(-10.5, 6.5) shouldBe -6.5 +- eps
    clip2(-10.5, 6.9) shouldBe -6.9 +- eps
    clip2(-10.5, -6.0) shouldBe 6.0 +- eps
    clip2(-10.5, -6.1) shouldBe 6.1 +- eps
    clip2(-10.5, -6.5) shouldBe 6.5 +- eps
    clip2(-10.5, -6.9) shouldBe 6.9 +- eps
    clip2(-10.9, 0.0) shouldBe -0.0 +- eps
    clip2(-10.9, 6.0) shouldBe -6.0 +- eps
    clip2(-10.9, 6.1) shouldBe -6.1 +- eps
    clip2(-10.9, 6.5) shouldBe -6.5 +- eps
    clip2(-10.9, 6.9) shouldBe -6.9 +- eps
    clip2(-10.9, -6.0) shouldBe 6.0 +- eps
    clip2(-10.9, -6.1) shouldBe 6.1 +- eps
    clip2(-10.9, -6.5) shouldBe 6.5 +- eps
    clip2(-10.9, -6.9) shouldBe 6.9 +- eps
  }

  "Double.excess" should work in {
    excess(0.0, 0.0) shouldBe 0.0 +- eps
    excess(0.0, 6.0) shouldBe 0.0 +- eps
    excess(0.0, 6.1) shouldBe 0.0 +- eps
    excess(0.0, 6.5) shouldBe 0.0 +- eps
    excess(0.0, 6.9) shouldBe 0.0 +- eps
    excess(0.0, -6.0) shouldBe -6.0 +- eps
    excess(0.0, -6.1) shouldBe -6.1 +- eps
    excess(0.0, -6.5) shouldBe -6.5 +- eps
    excess(0.0, -6.9) shouldBe -6.9 +- eps
    excess(10.0, 0.0) shouldBe 10.0 +- eps
    excess(10.0, 6.0) shouldBe 4.0 +- eps
    excess(10.0, 6.1) shouldBe 3.9 +- eps
    excess(10.0, 6.5) shouldBe 3.5 +- eps
    excess(10.0, 6.9) shouldBe 3.1 +- eps
    excess(10.0, -6.0) shouldBe 4.0 +- eps
    excess(10.0, -6.1) shouldBe 3.9 +- eps
    excess(10.0, -6.5) shouldBe 3.5 +- eps
    excess(10.0, -6.9) shouldBe 3.1 +- eps
    excess(10.1, 0.0) shouldBe 10.1 +- eps
    excess(10.1, 6.0) shouldBe 4.1 +- eps
    excess(10.1, 6.1) shouldBe 4.0 +- eps
    excess(10.1, 6.5) shouldBe 3.6 +- eps
    excess(10.1, 6.9) shouldBe 3.2 +- eps
    excess(10.1, -6.0) shouldBe 4.1 +- eps
    excess(10.1, -6.1) shouldBe 4.0 +- eps
    excess(10.1, -6.5) shouldBe 3.6 +- eps
    excess(10.1, -6.9) shouldBe 3.2 +- eps
    excess(10.5, 0.0) shouldBe 10.5 +- eps
    excess(10.5, 6.0) shouldBe 4.5 +- eps
    excess(10.5, 6.1) shouldBe 4.4 +- eps
    excess(10.5, 6.5) shouldBe 4.0 +- eps
    excess(10.5, 6.9) shouldBe 3.6 +- eps
    excess(10.5, -6.0) shouldBe 4.5 +- eps
    excess(10.5, -6.1) shouldBe 4.4 +- eps
    excess(10.5, -6.5) shouldBe 4.0 +- eps
    excess(10.5, -6.9) shouldBe 3.6 +- eps
    excess(10.9, 0.0) shouldBe 10.9 +- eps
    excess(10.9, 6.0) shouldBe 4.9 +- eps
    excess(10.9, 6.1) shouldBe 4.8 +- eps
    excess(10.9, 6.5) shouldBe 4.4 +- eps
    excess(10.9, 6.9) shouldBe 4.0 +- eps
    excess(10.9, -6.0) shouldBe 4.9 +- eps
    excess(10.9, -6.1) shouldBe 4.8 +- eps
    excess(10.9, -6.5) shouldBe 4.4 +- eps
    excess(10.9, -6.9) shouldBe 4.0 +- eps
    excess(-10.0, 0.0) shouldBe -10.0 +- eps
    excess(-10.0, 6.0) shouldBe -4.0 +- eps
    excess(-10.0, 6.1) shouldBe -3.9 +- eps
    excess(-10.0, 6.5) shouldBe -3.5 +- eps
    excess(-10.0, 6.9) shouldBe -3.1 +- eps
    excess(-10.0, -6.0) shouldBe -16.0 +- eps
    excess(-10.0, -6.1) shouldBe -16.1 +- eps
    excess(-10.0, -6.5) shouldBe -16.5 +- eps
    excess(-10.0, -6.9) shouldBe -16.9 +- eps
    excess(-10.1, 0.0) shouldBe -10.1 +- eps
    excess(-10.1, 6.0) shouldBe -4.1 +- eps
    excess(-10.1, 6.1) shouldBe -4.0 +- eps
    excess(-10.1, 6.5) shouldBe -3.6 +- eps
    excess(-10.1, 6.9) shouldBe -3.2 +- eps
    excess(-10.1, -6.0) shouldBe -16.1 +- eps
    excess(-10.1, -6.1) shouldBe -16.2 +- eps
    excess(-10.1, -6.5) shouldBe -16.6 +- eps
    excess(-10.1, -6.9) shouldBe -17.0 +- eps
    excess(-10.5, 0.0) shouldBe -10.5 +- eps
    excess(-10.5, 6.0) shouldBe -4.5 +- eps
    excess(-10.5, 6.1) shouldBe -4.4 +- eps
    excess(-10.5, 6.5) shouldBe -4.0 +- eps
    excess(-10.5, 6.9) shouldBe -3.6 +- eps
    excess(-10.5, -6.0) shouldBe -16.5 +- eps
    excess(-10.5, -6.1) shouldBe -16.6 +- eps
    excess(-10.5, -6.5) shouldBe -17.0 +- eps
    excess(-10.5, -6.9) shouldBe -17.4 +- eps
    excess(-10.9, 0.0) shouldBe -10.9 +- eps
    excess(-10.9, 6.0) shouldBe -4.9 +- eps
    excess(-10.9, 6.1) shouldBe -4.8 +- eps
    excess(-10.9, 6.5) shouldBe -4.4 +- eps
    excess(-10.9, 6.9) shouldBe -4.0 +- eps
    excess(-10.9, -6.0) shouldBe -16.9 +- eps
    excess(-10.9, -6.1) shouldBe -17.0 +- eps
    excess(-10.9, -6.5) shouldBe -17.4 +- eps
    excess(-10.9, -6.9) shouldBe -17.8 +- eps
  }

  "Double.fold2" should work in {
    fold2(0.0, 0.0) shouldBe 0.0 +- eps
    fold2(0.0, 6.0) shouldBe 0.0 +- eps
    fold2(0.0, 6.1) shouldBe 0.0 +- eps
    fold2(0.0, 6.5) shouldBe 0.0 +- eps
    fold2(0.0, 6.9) shouldBe 0.0 +- eps
    fold2(0.0, -6.0) shouldBe -12.0 +- eps
    fold2(0.0, -6.1) shouldBe -12.2 +- eps
    fold2(0.0, -6.5) shouldBe -13.0 +- eps
    fold2(0.0, -6.9) shouldBe -13.8 +- eps
    fold2(10.0, 0.0) shouldBe -0.0 +- eps
    fold2(10.0, 6.0) shouldBe 2.0 +- eps
    fold2(10.0, 6.1) shouldBe 2.2 +- eps
    fold2(10.0, 6.5) shouldBe 3.0 +- eps
    fold2(10.0, 6.9) shouldBe 3.8 +- eps
    fold2(10.0, -6.0) shouldBe -14.0 +- eps
    fold2(10.0, -6.1) shouldBe -14.4 +- eps
    fold2(10.0, -6.5) shouldBe -16.0 +- eps
    fold2(10.0, -6.9) shouldBe -17.6 +- eps
    fold2(10.1, 0.0) shouldBe -0.0 +- eps
    fold2(10.1, 6.0) shouldBe 1.9 +- eps
    fold2(10.1, 6.1) shouldBe 2.1 +- eps
    fold2(10.1, 6.5) shouldBe 2.9 +- eps
    fold2(10.1, 6.9) shouldBe 3.7 +- eps
    fold2(10.1, -6.0) shouldBe -13.9 +- eps
    fold2(10.1, -6.1) shouldBe -14.3 +- eps
    fold2(10.1, -6.5) shouldBe -15.9 +- eps
    fold2(10.1, -6.9) shouldBe -17.5 +- eps
    fold2(10.5, 0.0) shouldBe -0.0 +- eps
    fold2(10.5, 6.0) shouldBe 1.5 +- eps
    fold2(10.5, 6.1) shouldBe 1.7 +- eps
    fold2(10.5, 6.5) shouldBe 2.5 +- eps
    fold2(10.5, 6.9) shouldBe 3.3 +- eps
    fold2(10.5, -6.0) shouldBe -13.5 +- eps
    fold2(10.5, -6.1) shouldBe -13.9 +- eps
    fold2(10.5, -6.5) shouldBe -15.5 +- eps
    fold2(10.5, -6.9) shouldBe -17.1 +- eps
    fold2(10.9, 0.0) shouldBe -0.0 +- eps
    fold2(10.9, 6.0) shouldBe 1.1 +- eps
    fold2(10.9, 6.1) shouldBe 1.3 +- eps
    fold2(10.9, 6.5) shouldBe 2.1 +- eps
    fold2(10.9, 6.9) shouldBe 2.9 +- eps
    fold2(10.9, -6.0) shouldBe -13.1 +- eps
    fold2(10.9, -6.1) shouldBe -13.5 +- eps
    fold2(10.9, -6.5) shouldBe -15.1 +- eps
    fold2(10.9, -6.9) shouldBe -16.7 +- eps
    fold2(-10.0, 0.0) shouldBe -0.0 +- eps
    fold2(-10.0, 6.0) shouldBe -2.0 +- eps
    fold2(-10.0, 6.1) shouldBe -2.2 +- eps
    fold2(-10.0, 6.5) shouldBe -3.0 +- eps
    fold2(-10.0, 6.9) shouldBe -3.8 +- eps
    fold2(-10.0, -6.0) shouldBe -10.0 +- eps
    fold2(-10.0, -6.1) shouldBe -10.0 +- eps
    fold2(-10.0, -6.5) shouldBe -10.0 +- eps
    fold2(-10.0, -6.9) shouldBe -10.0 +- eps
    fold2(-10.1, 0.0) shouldBe -0.0 +- eps
    fold2(-10.1, 6.0) shouldBe -1.9 +- eps
    fold2(-10.1, 6.1) shouldBe -2.1 +- eps
    fold2(-10.1, 6.5) shouldBe -2.9 +- eps
    fold2(-10.1, 6.9) shouldBe -3.7 +- eps
    fold2(-10.1, -6.0) shouldBe -10.1 +- eps
    fold2(-10.1, -6.1) shouldBe -10.1 +- eps
    fold2(-10.1, -6.5) shouldBe -10.1 +- eps
    fold2(-10.1, -6.9) shouldBe -10.1 +- eps
    fold2(-10.5, 0.0) shouldBe -0.0 +- eps
    fold2(-10.5, 6.0) shouldBe -1.5 +- eps
    fold2(-10.5, 6.1) shouldBe -1.7 +- eps
    fold2(-10.5, 6.5) shouldBe -2.5 +- eps
    fold2(-10.5, 6.9) shouldBe -3.3 +- eps
    fold2(-10.5, -6.0) shouldBe -10.5 +- eps
    fold2(-10.5, -6.1) shouldBe -10.5 +- eps
    fold2(-10.5, -6.5) shouldBe -10.5 +- eps
    fold2(-10.5, -6.9) shouldBe -10.5 +- eps
    fold2(-10.9, 0.0) shouldBe -0.0 +- eps
    fold2(-10.9, 6.0) shouldBe -1.1 +- eps
    fold2(-10.9, 6.1) shouldBe -1.3 +- eps
    fold2(-10.9, 6.5) shouldBe -2.1 +- eps
    fold2(-10.9, 6.9) shouldBe -2.9 +- eps
    fold2(-10.9, -6.0) shouldBe -10.9 +- eps
    fold2(-10.9, -6.1) shouldBe -10.9 +- eps
    fold2(-10.9, -6.5) shouldBe -10.9 +- eps
    fold2(-10.9, -6.9) shouldBe -10.9 +- eps
  }

  "Double.wrap2" should work in {
    wrap2(0.0, 0.0) shouldBe -0.0 +- eps
    wrap2(0.0, 6.0) shouldBe 0.0 +- eps
    wrap2(0.0, 6.1) shouldBe 0.0 +- eps
    wrap2(0.0, 6.5) shouldBe 0.0 +- eps
    wrap2(0.0, 6.9) shouldBe 0.0 +- eps
    wrap2(0.0, -6.0) shouldBe 0.0 +- eps
    wrap2(0.0, -6.1) shouldBe 0.0 +- eps
    wrap2(0.0, -6.5) shouldBe 0.0 +- eps
    wrap2(0.0, -6.9) shouldBe 0.0 +- eps
    wrap2(10.0, 0.0) shouldBe -0.0 +- eps
    wrap2(10.0, 6.0) shouldBe -2.0 +- eps
    wrap2(10.0, 6.1) shouldBe -2.2 +- eps
    wrap2(10.0, 6.5) shouldBe -3.0 +- eps
    wrap2(10.0, 6.9) shouldBe -3.8 +- eps
    wrap2(10.0, -6.0) shouldBe -2.0 +- eps
    wrap2(10.0, -6.1) shouldBe -2.2 +- eps
    wrap2(10.0, -6.5) shouldBe -3.0 +- eps
    wrap2(10.0, -6.9) shouldBe -3.8 +- eps
    wrap2(10.1, 0.0) shouldBe -0.0 +- eps
    wrap2(10.1, 6.0) shouldBe -1.9 +- eps
    wrap2(10.1, 6.1) shouldBe -2.1 +- eps
    wrap2(10.1, 6.5) shouldBe -2.9 +- eps
    wrap2(10.1, 6.9) shouldBe -3.7 +- eps
    wrap2(10.1, -6.0) shouldBe -1.9 +- eps
    wrap2(10.1, -6.1) shouldBe -2.1 +- eps
    wrap2(10.1, -6.5) shouldBe -2.9 +- eps
    wrap2(10.1, -6.9) shouldBe -3.7 +- eps
    wrap2(10.5, 0.0) shouldBe -0.0 +- eps
    wrap2(10.5, 6.0) shouldBe -1.5 +- eps
    wrap2(10.5, 6.1) shouldBe -1.7 +- eps
    wrap2(10.5, 6.5) shouldBe -2.5 +- eps
    wrap2(10.5, 6.9) shouldBe -3.3 +- eps
    wrap2(10.5, -6.0) shouldBe -1.5 +- eps
    wrap2(10.5, -6.1) shouldBe -1.7 +- eps
    wrap2(10.5, -6.5) shouldBe -2.5 +- eps
    wrap2(10.5, -6.9) shouldBe -3.3 +- eps
    wrap2(10.9, 0.0) shouldBe -0.0 +- eps
    wrap2(10.9, 6.0) shouldBe -1.1 +- eps
    wrap2(10.9, 6.1) shouldBe -1.3 +- eps
    wrap2(10.9, 6.5) shouldBe -2.1 +- eps
    wrap2(10.9, 6.9) shouldBe -2.9 +- eps
    wrap2(10.9, -6.0) shouldBe -1.1 +- eps
    wrap2(10.9, -6.1) shouldBe -1.3 +- eps
    wrap2(10.9, -6.5) shouldBe -2.1 +- eps
    wrap2(10.9, -6.9) shouldBe -2.9 +- eps
    wrap2(-10.0, 0.0) shouldBe -0.0 +- eps
    wrap2(-10.0, 6.0) shouldBe 2.0 +- eps
    wrap2(-10.0, 6.1) shouldBe 2.2 +- eps
    wrap2(-10.0, 6.5) shouldBe 3.0 +- eps
    wrap2(-10.0, 6.9) shouldBe 3.8 +- eps
    wrap2(-10.0, -6.0) shouldBe 2.0 +- eps
    wrap2(-10.0, -6.1) shouldBe 2.2 +- eps
    wrap2(-10.0, -6.5) shouldBe 3.0 +- eps
    wrap2(-10.0, -6.9) shouldBe 3.8 +- eps
    wrap2(-10.1, 0.0) shouldBe -0.0 +- eps
    wrap2(-10.1, 6.0) shouldBe 1.9 +- eps
    wrap2(-10.1, 6.1) shouldBe 2.1 +- eps
    wrap2(-10.1, 6.5) shouldBe 2.9 +- eps
    wrap2(-10.1, 6.9) shouldBe 3.7 +- eps
    wrap2(-10.1, -6.0) shouldBe 1.9 +- eps
    wrap2(-10.1, -6.1) shouldBe 2.1 +- eps
    wrap2(-10.1, -6.5) shouldBe 2.9 +- eps
    wrap2(-10.1, -6.9) shouldBe 3.7 +- eps
    wrap2(-10.5, 0.0) shouldBe -0.0 +- eps
    wrap2(-10.5, 6.0) shouldBe 1.5 +- eps
    wrap2(-10.5, 6.1) shouldBe 1.7 +- eps
    wrap2(-10.5, 6.5) shouldBe 2.5 +- eps
    wrap2(-10.5, 6.9) shouldBe 3.3 +- eps
    wrap2(-10.5, -6.0) shouldBe 1.5 +- eps
    wrap2(-10.5, -6.1) shouldBe 1.7 +- eps
    wrap2(-10.5, -6.5) shouldBe 2.5 +- eps
    wrap2(-10.5, -6.9) shouldBe 3.3 +- eps
    wrap2(-10.9, 0.0) shouldBe -0.0 +- eps
    wrap2(-10.9, 6.0) shouldBe 1.1 +- eps
    wrap2(-10.9, 6.1) shouldBe 1.3 +- eps
    wrap2(-10.9, 6.5) shouldBe 2.1 +- eps
    wrap2(-10.9, 6.9) shouldBe 2.9 +- eps
    wrap2(-10.9, -6.0) shouldBe 1.1 +- eps
    wrap2(-10.9, -6.1) shouldBe 1.3 +- eps
    wrap2(-10.9, -6.5) shouldBe 2.1 +- eps
    wrap2(-10.9, -6.9) shouldBe 2.9 +- eps
  }

  "Double.mod" should work in {
    mod(0.0, 0.0) shouldBe 0.0 +- eps
    mod(0.0, 6.0) shouldBe 0.0 +- eps
    mod(0.0, 6.1) shouldBe 0.0 +- eps
    mod(0.0, 6.5) shouldBe 0.0 +- eps
    mod(0.0, 6.9) shouldBe 0.0 +- eps
    mod(0.0, -6.0) shouldBe 0.0 +- eps
    mod(0.0, -6.1) shouldBe 0.0 +- eps
    mod(0.0, -6.5) shouldBe 0.0 +- eps
    mod(0.0, -6.9) shouldBe 0.0 +- eps
    mod(10.0, 0.0) shouldBe 0.0 +- eps
    mod(10.0, 6.0) shouldBe 4.0 +- eps
    mod(10.0, 6.1) shouldBe 3.9 +- eps
    mod(10.0, 6.5) shouldBe 3.5 +- eps
    mod(10.0, 6.9) shouldBe 3.1 +- eps
    mod(10.0, -6.0) shouldBe -2.0 +- eps
    mod(10.0, -6.1) shouldBe -2.2 +- eps
    mod(10.0, -6.5) shouldBe -3.0 +- eps
    mod(10.0, -6.9) shouldBe -3.8 +- eps
    mod(10.1, 0.0) shouldBe 0.0 +- eps
    mod(10.1, 6.0) shouldBe 4.1 +- eps
    mod(10.1, 6.1) shouldBe 4.0 +- eps
    mod(10.1, 6.5) shouldBe 3.6 +- eps
    mod(10.1, 6.9) shouldBe 3.2 +- eps
    mod(10.1, -6.0) shouldBe -1.9 +- eps
    mod(10.1, -6.1) shouldBe -2.1 +- eps
    mod(10.1, -6.5) shouldBe -2.9 +- eps
    mod(10.1, -6.9) shouldBe -3.7 +- eps
    mod(10.5, 0.0) shouldBe 0.0 +- eps
    mod(10.5, 6.0) shouldBe 4.5 +- eps
    mod(10.5, 6.1) shouldBe 4.4 +- eps
    mod(10.5, 6.5) shouldBe 4.0 +- eps
    mod(10.5, 6.9) shouldBe 3.6 +- eps
    mod(10.5, -6.0) shouldBe -1.5 +- eps
    mod(10.5, -6.1) shouldBe -1.7 +- eps
    mod(10.5, -6.5) shouldBe -2.5 +- eps
    mod(10.5, -6.9) shouldBe -3.3 +- eps
    mod(10.9, 0.0) shouldBe 0.0 +- eps
    mod(10.9, 6.0) shouldBe 4.9 +- eps
    mod(10.9, 6.1) shouldBe 4.8 +- eps
    mod(10.9, 6.5) shouldBe 4.4 +- eps
    mod(10.9, 6.9) shouldBe 4.0 +- eps
    mod(10.9, -6.0) shouldBe -1.1 +- eps
    mod(10.9, -6.1) shouldBe -1.3 +- eps
    mod(10.9, -6.5) shouldBe -2.1 +- eps
    mod(10.9, -6.9) shouldBe -2.9 +- eps
    mod(-10.0, 0.0) shouldBe 0.0 +- eps
    mod(-10.0, 6.0) shouldBe 2.0 +- eps
    mod(-10.0, 6.1) shouldBe 2.2 +- eps
    mod(-10.0, 6.5) shouldBe 3.0 +- eps
    mod(-10.0, 6.9) shouldBe 3.8 +- eps
    mod(-10.0, -6.0) shouldBe -4.0 +- eps
    mod(-10.0, -6.1) shouldBe -3.9 +- eps
    mod(-10.0, -6.5) shouldBe -3.5 +- eps
    mod(-10.0, -6.9) shouldBe -3.1 +- eps
    mod(-10.1, 0.0) shouldBe 0.0 +- eps
    mod(-10.1, 6.0) shouldBe 1.9 +- eps
    mod(-10.1, 6.1) shouldBe 2.1 +- eps
    mod(-10.1, 6.5) shouldBe 2.9 +- eps
    mod(-10.1, 6.9) shouldBe 3.7 +- eps
    mod(-10.1, -6.0) shouldBe -4.1 +- eps
    mod(-10.1, -6.1) shouldBe -4.0 +- eps
    mod(-10.1, -6.5) shouldBe -3.6 +- eps
    mod(-10.1, -6.9) shouldBe -3.2 +- eps
    mod(-10.5, 0.0) shouldBe 0.0 +- eps
    mod(-10.5, 6.0) shouldBe 1.5 +- eps
    mod(-10.5, 6.1) shouldBe 1.7 +- eps
    mod(-10.5, 6.5) shouldBe 2.5 +- eps
    mod(-10.5, 6.9) shouldBe 3.3 +- eps
    mod(-10.5, -6.0) shouldBe -4.5 +- eps
    mod(-10.5, -6.1) shouldBe -4.4 +- eps
    mod(-10.5, -6.5) shouldBe -4.0 +- eps
    mod(-10.5, -6.9) shouldBe -3.6 +- eps
    mod(-10.9, 0.0) shouldBe 0.0 +- eps
    mod(-10.9, 6.0) shouldBe 1.1 +- eps
    mod(-10.9, 6.1) shouldBe 1.3 +- eps
    mod(-10.9, 6.5) shouldBe 2.1 +- eps
    mod(-10.9, 6.9) shouldBe 2.9 +- eps
    mod(-10.9, -6.0) shouldBe -4.9 +- eps
    mod(-10.9, -6.1) shouldBe -4.8 +- eps
    mod(-10.9, -6.5) shouldBe -4.4 +- eps
    mod(-10.9, -6.9) shouldBe -4.0 +- eps
  }

  "Double.ring1" should work in {
    ring1(0.0, 0.0) shouldBe 0.0 +- eps
    ring1(0.0, 6.0) shouldBe 0.0 +- eps
    ring1(0.0, 6.1) shouldBe 0.0 +- eps
    ring1(0.0, 6.5) shouldBe 0.0 +- eps
    ring1(0.0, 6.9) shouldBe 0.0 +- eps
    ring1(0.0, -6.0) shouldBe 0.0 +- eps
    ring1(0.0, -6.1) shouldBe 0.0 +- eps
    ring1(0.0, -6.5) shouldBe 0.0 +- eps
    ring1(0.0, -6.9) shouldBe 0.0 +- eps
    ring1(10.0, 0.0) shouldBe 10.0 +- eps
    ring1(10.0, 6.0) shouldBe 70.0 +- eps
    ring1(10.0, 6.1) shouldBe 71.0 +- eps
    ring1(10.0, 6.5) shouldBe 75.0 +- eps
    ring1(10.0, 6.9) shouldBe 79.0 +- eps
    ring1(10.0, -6.0) shouldBe -50.0 +- eps
    ring1(10.0, -6.1) shouldBe -51.0 +- eps
    ring1(10.0, -6.5) shouldBe -55.0 +- eps
    ring1(10.0, -6.9) shouldBe -59.0 +- eps
    ring1(10.1, 0.0) shouldBe 10.1 +- eps
    ring1(10.1, 6.0) shouldBe 70.7 +- eps
    ring1(10.1, 6.1) shouldBe 71.71 +- eps
    ring1(10.1, 6.5) shouldBe 75.75 +- eps
    ring1(10.1, 6.9) shouldBe 79.79 +- eps
    ring1(10.1, -6.0) shouldBe -50.5 +- eps
    ring1(10.1, -6.1) shouldBe -51.51 +- eps
    ring1(10.1, -6.5) shouldBe -55.55 +- eps
    ring1(10.1, -6.9) shouldBe -59.59 +- eps
    ring1(10.5, 0.0) shouldBe 10.5 +- eps
    ring1(10.5, 6.0) shouldBe 73.5 +- eps
    ring1(10.5, 6.1) shouldBe 74.55 +- eps
    ring1(10.5, 6.5) shouldBe 78.75 +- eps
    ring1(10.5, 6.9) shouldBe 82.95 +- eps
    ring1(10.5, -6.0) shouldBe -52.5 +- eps
    ring1(10.5, -6.1) shouldBe -53.55 +- eps
    ring1(10.5, -6.5) shouldBe -57.75 +- eps
    ring1(10.5, -6.9) shouldBe -61.95 +- eps
    ring1(10.9, 0.0) shouldBe 10.9 +- eps
    ring1(10.9, 6.0) shouldBe 76.3 +- eps
    ring1(10.9, 6.1) shouldBe 77.39 +- eps
    ring1(10.9, 6.5) shouldBe 81.75 +- eps
    ring1(10.9, 6.9) shouldBe 86.11 +- eps
    ring1(10.9, -6.0) shouldBe -54.5 +- eps
    ring1(10.9, -6.1) shouldBe -55.59 +- eps
    ring1(10.9, -6.5) shouldBe -59.95 +- eps
    ring1(10.9, -6.9) shouldBe -64.31 +- eps
    ring1(-10.0, 0.0) shouldBe -10.0 +- eps
    ring1(-10.0, 6.0) shouldBe -70.0 +- eps
    ring1(-10.0, 6.1) shouldBe -71.0 +- eps
    ring1(-10.0, 6.5) shouldBe -75.0 +- eps
    ring1(-10.0, 6.9) shouldBe -79.0 +- eps
    ring1(-10.0, -6.0) shouldBe 50.0 +- eps
    ring1(-10.0, -6.1) shouldBe 51.0 +- eps
    ring1(-10.0, -6.5) shouldBe 55.0 +- eps
    ring1(-10.0, -6.9) shouldBe 59.0 +- eps
    ring1(-10.1, 0.0) shouldBe -10.1 +- eps
    ring1(-10.1, 6.0) shouldBe -70.7 +- eps
    ring1(-10.1, 6.1) shouldBe -71.71 +- eps
    ring1(-10.1, 6.5) shouldBe -75.75 +- eps
    ring1(-10.1, 6.9) shouldBe -79.79 +- eps
    ring1(-10.1, -6.0) shouldBe 50.5 +- eps
    ring1(-10.1, -6.1) shouldBe 51.51 +- eps
    ring1(-10.1, -6.5) shouldBe 55.55 +- eps
    ring1(-10.1, -6.9) shouldBe 59.59 +- eps
    ring1(-10.5, 0.0) shouldBe -10.5 +- eps
    ring1(-10.5, 6.0) shouldBe -73.5 +- eps
    ring1(-10.5, 6.1) shouldBe -74.55 +- eps
    ring1(-10.5, 6.5) shouldBe -78.75 +- eps
    ring1(-10.5, 6.9) shouldBe -82.95 +- eps
    ring1(-10.5, -6.0) shouldBe 52.5 +- eps
    ring1(-10.5, -6.1) shouldBe 53.55 +- eps
    ring1(-10.5, -6.5) shouldBe 57.75 +- eps
    ring1(-10.5, -6.9) shouldBe 61.95 +- eps
    ring1(-10.9, 0.0) shouldBe -10.9 +- eps
    ring1(-10.9, 6.0) shouldBe -76.3 +- eps
    ring1(-10.9, 6.1) shouldBe -77.39 +- eps
    ring1(-10.9, 6.5) shouldBe -81.75 +- eps
    ring1(-10.9, 6.9) shouldBe -86.11 +- eps
    ring1(-10.9, -6.0) shouldBe 54.5 +- eps
    ring1(-10.9, -6.1) shouldBe 55.59 +- eps
    ring1(-10.9, -6.5) shouldBe 59.95 +- eps
    ring1(-10.9, -6.9) shouldBe 64.31 +- eps
  }

  "Double.ring2" should work in {
    ring2(0.0, 0.0) shouldBe 0.0 +- eps
    ring2(0.0, 6.0) shouldBe 6.0 +- eps
    ring2(0.0, 6.1) shouldBe 6.1 +- eps
    ring2(0.0, 6.5) shouldBe 6.5 +- eps
    ring2(0.0, 6.9) shouldBe 6.9 +- eps
    ring2(0.0, -6.0) shouldBe -6.0 +- eps
    ring2(0.0, -6.1) shouldBe -6.1 +- eps
    ring2(0.0, -6.5) shouldBe -6.5 +- eps
    ring2(0.0, -6.9) shouldBe -6.9 +- eps
    ring2(10.0, 0.0) shouldBe 10.0 +- eps
    ring2(10.0, 6.0) shouldBe 76.0 +- eps
    ring2(10.0, 6.1) shouldBe 77.1 +- eps
    ring2(10.0, 6.5) shouldBe 81.5 +- eps
    ring2(10.0, 6.9) shouldBe 85.9 +- eps
    ring2(10.0, -6.0) shouldBe -56.0 +- eps
    ring2(10.0, -6.1) shouldBe -57.1 +- eps
    ring2(10.0, -6.5) shouldBe -61.5 +- eps
    ring2(10.0, -6.9) shouldBe -65.9 +- eps
    ring2(10.1, 0.0) shouldBe 10.1 +- eps
    ring2(10.1, 6.0) shouldBe 76.7 +- eps
    ring2(10.1, 6.1) shouldBe 77.81 +- eps
    ring2(10.1, 6.5) shouldBe 82.25 +- eps
    ring2(10.1, 6.9) shouldBe 86.69 +- eps
    ring2(10.1, -6.0) shouldBe -56.5 +- eps
    ring2(10.1, -6.1) shouldBe -57.61 +- eps
    ring2(10.1, -6.5) shouldBe -62.05 +- eps
    ring2(10.1, -6.9) shouldBe -66.49 +- eps
    ring2(10.5, 0.0) shouldBe 10.5 +- eps
    ring2(10.5, 6.0) shouldBe 79.5 +- eps
    ring2(10.5, 6.1) shouldBe 80.65 +- eps
    ring2(10.5, 6.5) shouldBe 85.25 +- eps
    ring2(10.5, 6.9) shouldBe 89.85 +- eps
    ring2(10.5, -6.0) shouldBe -58.5 +- eps
    ring2(10.5, -6.1) shouldBe -59.65 +- eps
    ring2(10.5, -6.5) shouldBe -64.25 +- eps
    ring2(10.5, -6.9) shouldBe -68.85 +- eps
    ring2(10.9, 0.0) shouldBe 10.9 +- eps
    ring2(10.9, 6.0) shouldBe 82.3 +- eps
    ring2(10.9, 6.1) shouldBe 83.49 +- eps
    ring2(10.9, 6.5) shouldBe 88.25 +- eps
    ring2(10.9, 6.9) shouldBe 93.01 +- eps
    ring2(10.9, -6.0) shouldBe -60.5 +- eps
    ring2(10.9, -6.1) shouldBe -61.69 +- eps
    ring2(10.9, -6.5) shouldBe -66.45 +- eps
    ring2(10.9, -6.9) shouldBe -71.21 +- eps
    ring2(-10.0, 0.0) shouldBe -10.0 +- eps
    ring2(-10.0, 6.0) shouldBe -64.0 +- eps
    ring2(-10.0, 6.1) shouldBe -64.9 +- eps
    ring2(-10.0, 6.5) shouldBe -68.5 +- eps
    ring2(-10.0, 6.9) shouldBe -72.1 +- eps
    ring2(-10.0, -6.0) shouldBe 44.0 +- eps
    ring2(-10.0, -6.1) shouldBe 44.9 +- eps
    ring2(-10.0, -6.5) shouldBe 48.5 +- eps
    ring2(-10.0, -6.9) shouldBe 52.1 +- eps
    ring2(-10.1, 0.0) shouldBe -10.1 +- eps
    ring2(-10.1, 6.0) shouldBe -64.7 +- eps
    ring2(-10.1, 6.1) shouldBe -65.61 +- eps
    ring2(-10.1, 6.5) shouldBe -69.25 +- eps
    ring2(-10.1, 6.9) shouldBe -72.89 +- eps
    ring2(-10.1, -6.0) shouldBe 44.5 +- eps
    ring2(-10.1, -6.1) shouldBe 45.41 +- eps
    ring2(-10.1, -6.5) shouldBe 49.05 +- eps
    ring2(-10.1, -6.9) shouldBe 52.69 +- eps
    ring2(-10.5, 0.0) shouldBe -10.5 +- eps
    ring2(-10.5, 6.0) shouldBe -67.5 +- eps
    ring2(-10.5, 6.1) shouldBe -68.45 +- eps
    ring2(-10.5, 6.5) shouldBe -72.25 +- eps
    ring2(-10.5, 6.9) shouldBe -76.05 +- eps
    ring2(-10.5, -6.0) shouldBe 46.5 +- eps
    ring2(-10.5, -6.1) shouldBe 47.45 +- eps
    ring2(-10.5, -6.5) shouldBe 51.25 +- eps
    ring2(-10.5, -6.9) shouldBe 55.05 +- eps
    ring2(-10.9, 0.0) shouldBe -10.9 +- eps
    ring2(-10.9, 6.0) shouldBe -70.3 +- eps
    ring2(-10.9, 6.1) shouldBe -71.29 +- eps
    ring2(-10.9, 6.5) shouldBe -75.25 +- eps
    ring2(-10.9, 6.9) shouldBe -79.21 +- eps
    ring2(-10.9, -6.0) shouldBe 48.5 +- eps
    ring2(-10.9, -6.1) shouldBe 49.49 +- eps
    ring2(-10.9, -6.5) shouldBe 53.45 +- eps
    ring2(-10.9, -6.9) shouldBe 57.41 +- eps
  }

  "Double.ring3" should work in {
    ring3(0.0, 0.0) shouldBe 0.0 +- eps
    ring3(0.0, 6.0) shouldBe 0.0 +- eps
    ring3(0.0, 6.1) shouldBe 0.0 +- eps
    ring3(0.0, 6.5) shouldBe 0.0 +- eps
    ring3(0.0, 6.9) shouldBe 0.0 +- eps
    ring3(0.0, -6.0) shouldBe -0.0 +- eps
    ring3(0.0, -6.1) shouldBe -0.0 +- eps
    ring3(0.0, -6.5) shouldBe -0.0 +- eps
    ring3(0.0, -6.9) shouldBe -0.0 +- eps
    ring3(10.0, 0.0) shouldBe 0.0 +- eps
    ring3(10.0, 6.0) shouldBe 600.0 +- eps
    ring3(10.0, 6.1) shouldBe 610.0 +- eps
    ring3(10.0, 6.5) shouldBe 650.0 +- eps
    ring3(10.0, 6.9) shouldBe 690.0 +- eps
    ring3(10.0, -6.0) shouldBe -600.0 +- eps
    ring3(10.0, -6.1) shouldBe -610.0 +- eps
    ring3(10.0, -6.5) shouldBe -650.0 +- eps
    ring3(10.0, -6.9) shouldBe -690.0 +- eps
    ring3(10.1, 0.0) shouldBe 0.0 +- eps
    ring3(10.1, 6.0) shouldBe 612.06 +- eps
    ring3(10.1, 6.1) shouldBe 622.261 +- eps
    ring3(10.1, 6.5) shouldBe 663.065 +- eps
    ring3(10.1, 6.9) shouldBe 703.869 +- eps
    ring3(10.1, -6.0) shouldBe -612.06 +- eps
    ring3(10.1, -6.1) shouldBe -622.261 +- eps
    ring3(10.1, -6.5) shouldBe -663.065 +- eps
    ring3(10.1, -6.9) shouldBe -703.869 +- eps
    ring3(10.5, 0.0) shouldBe 0.0 +- eps
    ring3(10.5, 6.0) shouldBe 661.5 +- eps
    ring3(10.5, 6.1) shouldBe 672.525 +- eps
    ring3(10.5, 6.5) shouldBe 716.625 +- eps
    ring3(10.5, 6.9) shouldBe 760.725 +- eps
    ring3(10.5, -6.0) shouldBe -661.5 +- eps
    ring3(10.5, -6.1) shouldBe -672.525 +- eps
    ring3(10.5, -6.5) shouldBe -716.625 +- eps
    ring3(10.5, -6.9) shouldBe -760.725 +- eps
    ring3(10.9, 0.0) shouldBe 0.0 +- eps
    ring3(10.9, 6.0) shouldBe 712.86 +- eps
    ring3(10.9, 6.1) shouldBe 724.741 +- eps
    ring3(10.9, 6.5) shouldBe 772.265 +- eps
    ring3(10.9, 6.9) shouldBe 819.789 +- eps
    ring3(10.9, -6.0) shouldBe -712.86 +- eps
    ring3(10.9, -6.1) shouldBe -724.741 +- eps
    ring3(10.9, -6.5) shouldBe -772.265 +- eps
    ring3(10.9, -6.9) shouldBe -819.789 +- eps
    ring3(-10.0, 0.0) shouldBe 0.0 +- eps
    ring3(-10.0, 6.0) shouldBe 600.0 +- eps
    ring3(-10.0, 6.1) shouldBe 610.0 +- eps
    ring3(-10.0, 6.5) shouldBe 650.0 +- eps
    ring3(-10.0, 6.9) shouldBe 690.0 +- eps
    ring3(-10.0, -6.0) shouldBe -600.0 +- eps
    ring3(-10.0, -6.1) shouldBe -610.0 +- eps
    ring3(-10.0, -6.5) shouldBe -650.0 +- eps
    ring3(-10.0, -6.9) shouldBe -690.0 +- eps
    ring3(-10.1, 0.0) shouldBe 0.0 +- eps
    ring3(-10.1, 6.0) shouldBe 612.06 +- eps
    ring3(-10.1, 6.1) shouldBe 622.261 +- eps
    ring3(-10.1, 6.5) shouldBe 663.065 +- eps
    ring3(-10.1, 6.9) shouldBe 703.869 +- eps
    ring3(-10.1, -6.0) shouldBe -612.06 +- eps
    ring3(-10.1, -6.1) shouldBe -622.261 +- eps
    ring3(-10.1, -6.5) shouldBe -663.065 +- eps
    ring3(-10.1, -6.9) shouldBe -703.869 +- eps
    ring3(-10.5, 0.0) shouldBe 0.0 +- eps
    ring3(-10.5, 6.0) shouldBe 661.5 +- eps
    ring3(-10.5, 6.1) shouldBe 672.525 +- eps
    ring3(-10.5, 6.5) shouldBe 716.625 +- eps
    ring3(-10.5, 6.9) shouldBe 760.725 +- eps
    ring3(-10.5, -6.0) shouldBe -661.5 +- eps
    ring3(-10.5, -6.1) shouldBe -672.525 +- eps
    ring3(-10.5, -6.5) shouldBe -716.625 +- eps
    ring3(-10.5, -6.9) shouldBe -760.725 +- eps
    ring3(-10.9, 0.0) shouldBe 0.0 +- eps
    ring3(-10.9, 6.0) shouldBe 712.86 +- eps
    ring3(-10.9, 6.1) shouldBe 724.741 +- eps
    ring3(-10.9, 6.5) shouldBe 772.265 +- eps
    ring3(-10.9, 6.9) shouldBe 819.789 +- eps
    ring3(-10.9, -6.0) shouldBe -712.86 +- eps
    ring3(-10.9, -6.1) shouldBe -724.741 +- eps
    ring3(-10.9, -6.5) shouldBe -772.265 +- eps
    ring3(-10.9, -6.9) shouldBe -819.789 +- eps
  }

  "Double.ring4" should work in {
    ring4(0.0, 0.0) shouldBe 0.0 +- eps
    ring4(0.0, 6.0) shouldBe -0.0 +- eps
    ring4(0.0, 6.1) shouldBe -0.0 +- eps
    ring4(0.0, 6.5) shouldBe -0.0 +- eps
    ring4(0.0, 6.9) shouldBe -0.0 +- eps
    ring4(0.0, -6.0) shouldBe -0.0 +- eps
    ring4(0.0, -6.1) shouldBe -0.0 +- eps
    ring4(0.0, -6.5) shouldBe -0.0 +- eps
    ring4(0.0, -6.9) shouldBe -0.0 +- eps
    ring4(10.0, 0.0) shouldBe 0.0 +- eps
    ring4(10.0, 6.0) shouldBe 240.0 +- eps
    ring4(10.0, 6.1) shouldBe 237.9 +- eps
    ring4(10.0, 6.5) shouldBe 227.5 +- eps
    ring4(10.0, 6.9) shouldBe 213.9 +- eps
    ring4(10.0, -6.0) shouldBe -960.0 +- eps
    ring4(10.0, -6.1) shouldBe -982.1 +- eps
    ring4(10.0, -6.5) shouldBe -1072.5 +- eps
    ring4(10.0, -6.9) shouldBe -1166.1 +- eps
    ring4(10.1, 0.0) shouldBe 0.0 +- eps
    ring4(10.1, 6.0) shouldBe 248.46 +- eps
    ring4(10.1, 6.1) shouldBe 246.44 +- eps
    ring4(10.1, 6.5) shouldBe 236.34 +- eps
    ring4(10.1, 6.9) shouldBe 223.008 +- eps
    ring4(10.1, -6.0) shouldBe -975.66 +- eps
    ring4(10.1, -6.1) shouldBe -998.082 +- eps
    ring4(10.1, -6.5) shouldBe -1089.79 +- eps
    ring4(10.1, -6.9) shouldBe -1184.73 +- eps
    ring4(10.5, 0.0) shouldBe 0.0 +- eps
    ring4(10.5, 6.0) shouldBe 283.5 +- eps
    ring4(10.5, 6.1) shouldBe 281.82 +- eps
    ring4(10.5, 6.5) shouldBe 273.0 +- eps
    ring4(10.5, 6.9) shouldBe 260.82 +- eps
    ring4(10.5, -6.0) shouldBe -1039.5 +- eps
    ring4(10.5, -6.1) shouldBe -1063.23 +- eps
    ring4(10.5, -6.5) shouldBe -1160.25 +- eps
    ring4(10.5, -6.9) shouldBe -1260.63 +- eps
    ring4(10.9, 0.0) shouldBe 0.0 +- eps
    ring4(10.9, 6.0) shouldBe 320.46 +- eps
    ring4(10.9, 6.1) shouldBe 319.152 +- eps
    ring4(10.9, 6.5) shouldBe 311.74 +- eps
    ring4(10.9, 6.9) shouldBe 300.84 +- eps
    ring4(10.9, -6.0) shouldBe -1105.26 +- eps
    ring4(10.9, -6.1) shouldBe -1130.33 +- eps
    ring4(10.9, -6.5) shouldBe -1232.79 +- eps
    ring4(10.9, -6.9) shouldBe -1338.738 +- eps
    ring4(-10.0, 0.0) shouldBe 0.0 +- eps
    ring4(-10.0, 6.0) shouldBe 960.0 +- eps
    ring4(-10.0, 6.1) shouldBe 982.1 +- eps
    ring4(-10.0, 6.5) shouldBe 1072.5 +- eps
    ring4(-10.0, 6.9) shouldBe 1166.1 +- eps
    ring4(-10.0, -6.0) shouldBe -240.0 +- eps
    ring4(-10.0, -6.1) shouldBe -237.9 +- eps
    ring4(-10.0, -6.5) shouldBe -227.5 +- eps
    ring4(-10.0, -6.9) shouldBe -213.9 +- eps
    ring4(-10.1, 0.0) shouldBe 0.0 +- eps
    ring4(-10.1, 6.0) shouldBe 975.66 +- eps
    ring4(-10.1, 6.1) shouldBe 998.082 +- eps
    ring4(-10.1, 6.5) shouldBe 1089.79 +- eps
    ring4(-10.1, 6.9) shouldBe 1184.73 +- eps
    ring4(-10.1, -6.0) shouldBe -248.46 +- eps
    ring4(-10.1, -6.1) shouldBe -246.44 +- eps
    ring4(-10.1, -6.5) shouldBe -236.34 +- eps
    ring4(-10.1, -6.9) shouldBe -223.008 +- eps
    ring4(-10.5, 0.0) shouldBe 0.0 +- eps
    ring4(-10.5, 6.0) shouldBe 1039.5 +- eps
    ring4(-10.5, 6.1) shouldBe 1063.23 +- eps
    ring4(-10.5, 6.5) shouldBe 1160.25 +- eps
    ring4(-10.5, 6.9) shouldBe 1260.63 +- eps
    ring4(-10.5, -6.0) shouldBe -283.5 +- eps
    ring4(-10.5, -6.1) shouldBe -281.82 +- eps
    ring4(-10.5, -6.5) shouldBe -273.0 +- eps
    ring4(-10.5, -6.9) shouldBe -260.82 +- eps
    ring4(-10.9, 0.0) shouldBe 0.0 +- eps
    ring4(-10.9, 6.0) shouldBe 1105.26 +- eps
    ring4(-10.9, 6.1) shouldBe 1130.33 +- eps
    ring4(-10.9, 6.5) shouldBe 1232.79 +- eps
    ring4(-10.9, 6.9) shouldBe 1338.738 +- eps
    ring4(-10.9, -6.0) shouldBe -320.46 +- eps
    ring4(-10.9, -6.1) shouldBe -319.152 +- eps
    ring4(-10.9, -6.5) shouldBe -311.74 +- eps
    ring4(-10.9, -6.9) shouldBe -300.84 +- eps
  }

  "Double.thresh" should work in {
    thresh(0.0, 0.0) shouldBe 0.0 +- eps
    thresh(0.0, 6.0) shouldBe 0.0 +- eps
    thresh(0.0, 6.1) shouldBe 0.0 +- eps
    thresh(0.0, 6.5) shouldBe 0.0 +- eps
    thresh(0.0, 6.9) shouldBe 0.0 +- eps
    thresh(0.0, -6.0) shouldBe 0.0 +- eps
    thresh(0.0, -6.1) shouldBe 0.0 +- eps
    thresh(0.0, -6.5) shouldBe 0.0 +- eps
    thresh(0.0, -6.9) shouldBe 0.0 +- eps
    thresh(10.0, 0.0) shouldBe 10.0 +- eps
    thresh(10.0, 6.0) shouldBe 10.0 +- eps
    thresh(10.0, 6.1) shouldBe 10.0 +- eps
    thresh(10.0, 6.5) shouldBe 10.0 +- eps
    thresh(10.0, 6.9) shouldBe 10.0 +- eps
    thresh(10.0, -6.0) shouldBe 10.0 +- eps
    thresh(10.0, -6.1) shouldBe 10.0 +- eps
    thresh(10.0, -6.5) shouldBe 10.0 +- eps
    thresh(10.0, -6.9) shouldBe 10.0 +- eps
    thresh(10.1, 0.0) shouldBe 10.1 +- eps
    thresh(10.1, 6.0) shouldBe 10.1 +- eps
    thresh(10.1, 6.1) shouldBe 10.1 +- eps
    thresh(10.1, 6.5) shouldBe 10.1 +- eps
    thresh(10.1, 6.9) shouldBe 10.1 +- eps
    thresh(10.1, -6.0) shouldBe 10.1 +- eps
    thresh(10.1, -6.1) shouldBe 10.1 +- eps
    thresh(10.1, -6.5) shouldBe 10.1 +- eps
    thresh(10.1, -6.9) shouldBe 10.1 +- eps
    thresh(10.5, 0.0) shouldBe 10.5 +- eps
    thresh(10.5, 6.0) shouldBe 10.5 +- eps
    thresh(10.5, 6.1) shouldBe 10.5 +- eps
    thresh(10.5, 6.5) shouldBe 10.5 +- eps
    thresh(10.5, 6.9) shouldBe 10.5 +- eps
    thresh(10.5, -6.0) shouldBe 10.5 +- eps
    thresh(10.5, -6.1) shouldBe 10.5 +- eps
    thresh(10.5, -6.5) shouldBe 10.5 +- eps
    thresh(10.5, -6.9) shouldBe 10.5 +- eps
    thresh(10.9, 0.0) shouldBe 10.9 +- eps
    thresh(10.9, 6.0) shouldBe 10.9 +- eps
    thresh(10.9, 6.1) shouldBe 10.9 +- eps
    thresh(10.9, 6.5) shouldBe 10.9 +- eps
    thresh(10.9, 6.9) shouldBe 10.9 +- eps
    thresh(10.9, -6.0) shouldBe 10.9 +- eps
    thresh(10.9, -6.1) shouldBe 10.9 +- eps
    thresh(10.9, -6.5) shouldBe 10.9 +- eps
    thresh(10.9, -6.9) shouldBe 10.9 +- eps
    thresh(-10.0, 0.0) shouldBe 0.0 +- eps
    thresh(-10.0, 6.0) shouldBe 0.0 +- eps
    thresh(-10.0, 6.1) shouldBe 0.0 +- eps
    thresh(-10.0, 6.5) shouldBe 0.0 +- eps
    thresh(-10.0, 6.9) shouldBe 0.0 +- eps
    thresh(-10.0, -6.0) shouldBe 0.0 +- eps
    thresh(-10.0, -6.1) shouldBe 0.0 +- eps
    thresh(-10.0, -6.5) shouldBe 0.0 +- eps
    thresh(-10.0, -6.9) shouldBe 0.0 +- eps
    thresh(-10.1, 0.0) shouldBe 0.0 +- eps
    thresh(-10.1, 6.0) shouldBe 0.0 +- eps
    thresh(-10.1, 6.1) shouldBe 0.0 +- eps
    thresh(-10.1, 6.5) shouldBe 0.0 +- eps
    thresh(-10.1, 6.9) shouldBe 0.0 +- eps
    thresh(-10.1, -6.0) shouldBe 0.0 +- eps
    thresh(-10.1, -6.1) shouldBe 0.0 +- eps
    thresh(-10.1, -6.5) shouldBe 0.0 +- eps
    thresh(-10.1, -6.9) shouldBe 0.0 +- eps
    thresh(-10.5, 0.0) shouldBe 0.0 +- eps
    thresh(-10.5, 6.0) shouldBe 0.0 +- eps
    thresh(-10.5, 6.1) shouldBe 0.0 +- eps
    thresh(-10.5, 6.5) shouldBe 0.0 +- eps
    thresh(-10.5, 6.9) shouldBe 0.0 +- eps
    thresh(-10.5, -6.0) shouldBe 0.0 +- eps
    thresh(-10.5, -6.1) shouldBe 0.0 +- eps
    thresh(-10.5, -6.5) shouldBe 0.0 +- eps
    thresh(-10.5, -6.9) shouldBe 0.0 +- eps
    thresh(-10.9, 0.0) shouldBe 0.0 +- eps
    thresh(-10.9, 6.0) shouldBe 0.0 +- eps
    thresh(-10.9, 6.1) shouldBe 0.0 +- eps
    thresh(-10.9, 6.5) shouldBe 0.0 +- eps
    thresh(-10.9, 6.9) shouldBe 0.0 +- eps
    thresh(-10.9, -6.0) shouldBe 0.0 +- eps
    thresh(-10.9, -6.1) shouldBe 0.0 +- eps
    thresh(-10.9, -6.5) shouldBe 0.0 +- eps
    thresh(-10.9, -6.9) shouldBe 0.0 +- eps
  }

  "Double.amclip" should work in {
    amClip(0.0, 0.0) shouldBe 0.0 +- eps
    amClip(0.0, 6.0) shouldBe 0.0 +- eps
    amClip(0.0, 6.1) shouldBe 0.0 +- eps
    amClip(0.0, 6.5) shouldBe 0.0 +- eps
    amClip(0.0, 6.9) shouldBe 0.0 +- eps
    amClip(0.0, -6.0) shouldBe 0.0 +- eps
    amClip(0.0, -6.1) shouldBe 0.0 +- eps
    amClip(0.0, -6.5) shouldBe 0.0 +- eps
    amClip(0.0, -6.9) shouldBe 0.0 +- eps
    amClip(10.0, 0.0) shouldBe 0.0 +- eps
    amClip(10.0, 6.0) shouldBe 60.0 +- eps
    amClip(10.0, 6.1) shouldBe 61.0 +- eps
    amClip(10.0, 6.5) shouldBe 65.0 +- eps
    amClip(10.0, 6.9) shouldBe 69.0 +- eps
    amClip(10.0, -6.0) shouldBe 0.0 +- eps
    amClip(10.0, -6.1) shouldBe 0.0 +- eps
    amClip(10.0, -6.5) shouldBe 0.0 +- eps
    amClip(10.0, -6.9) shouldBe 0.0 +- eps
    amClip(10.1, 0.0) shouldBe 0.0 +- eps
    amClip(10.1, 6.0) shouldBe 60.6 +- eps
    amClip(10.1, 6.1) shouldBe 61.61 +- eps
    amClip(10.1, 6.5) shouldBe 65.65 +- eps
    amClip(10.1, 6.9) shouldBe 69.69 +- eps
    amClip(10.1, -6.0) shouldBe 0.0 +- eps
    amClip(10.1, -6.1) shouldBe 0.0 +- eps
    amClip(10.1, -6.5) shouldBe 0.0 +- eps
    amClip(10.1, -6.9) shouldBe 0.0 +- eps
    amClip(10.5, 0.0) shouldBe 0.0 +- eps
    amClip(10.5, 6.0) shouldBe 63.0 +- eps
    amClip(10.5, 6.1) shouldBe 64.05 +- eps
    amClip(10.5, 6.5) shouldBe 68.25 +- eps
    amClip(10.5, 6.9) shouldBe 72.45 +- eps
    amClip(10.5, -6.0) shouldBe 0.0 +- eps
    amClip(10.5, -6.1) shouldBe 0.0 +- eps
    amClip(10.5, -6.5) shouldBe 0.0 +- eps
    amClip(10.5, -6.9) shouldBe 0.0 +- eps
    amClip(10.9, 0.0) shouldBe 0.0 +- eps
    amClip(10.9, 6.0) shouldBe 65.4 +- eps
    amClip(10.9, 6.1) shouldBe 66.49 +- eps
    amClip(10.9, 6.5) shouldBe 70.85 +- eps
    amClip(10.9, 6.9) shouldBe 75.21 +- eps
    amClip(10.9, -6.0) shouldBe 0.0 +- eps
    amClip(10.9, -6.1) shouldBe 0.0 +- eps
    amClip(10.9, -6.5) shouldBe 0.0 +- eps
    amClip(10.9, -6.9) shouldBe 0.0 +- eps
    amClip(-10.0, 0.0) shouldBe -0.0 +- eps
    amClip(-10.0, 6.0) shouldBe -60.0 +- eps
    amClip(-10.0, 6.1) shouldBe -61.0 +- eps
    amClip(-10.0, 6.5) shouldBe -65.0 +- eps
    amClip(-10.0, 6.9) shouldBe -69.0 +- eps
    amClip(-10.0, -6.0) shouldBe -0.0 +- eps
    amClip(-10.0, -6.1) shouldBe -0.0 +- eps
    amClip(-10.0, -6.5) shouldBe -0.0 +- eps
    amClip(-10.0, -6.9) shouldBe -0.0 +- eps
    amClip(-10.1, 0.0) shouldBe -0.0 +- eps
    amClip(-10.1, 6.0) shouldBe -60.6 +- eps
    amClip(-10.1, 6.1) shouldBe -61.61 +- eps
    amClip(-10.1, 6.5) shouldBe -65.65 +- eps
    amClip(-10.1, 6.9) shouldBe -69.69 +- eps
    amClip(-10.1, -6.0) shouldBe -0.0 +- eps
    amClip(-10.1, -6.1) shouldBe -0.0 +- eps
    amClip(-10.1, -6.5) shouldBe -0.0 +- eps
    amClip(-10.1, -6.9) shouldBe -0.0 +- eps
    amClip(-10.5, 0.0) shouldBe -0.0 +- eps
    amClip(-10.5, 6.0) shouldBe -63.0 +- eps
    amClip(-10.5, 6.1) shouldBe -64.05 +- eps
    amClip(-10.5, 6.5) shouldBe -68.25 +- eps
    amClip(-10.5, 6.9) shouldBe -72.45 +- eps
    amClip(-10.5, -6.0) shouldBe -0.0 +- eps
    amClip(-10.5, -6.1) shouldBe -0.0 +- eps
    amClip(-10.5, -6.5) shouldBe -0.0 +- eps
    amClip(-10.5, -6.9) shouldBe -0.0 +- eps
    amClip(-10.9, 0.0) shouldBe -0.0 +- eps
    amClip(-10.9, 6.0) shouldBe -65.4 +- eps
    amClip(-10.9, 6.1) shouldBe -66.49 +- eps
    amClip(-10.9, 6.5) shouldBe -70.85 +- eps
    amClip(-10.9, 6.9) shouldBe -75.21 +- eps
    amClip(-10.9, -6.0) shouldBe -0.0 +- eps
    amClip(-10.9, -6.1) shouldBe -0.0 +- eps
    amClip(-10.9, -6.5) shouldBe -0.0 +- eps
    amClip(-10.9, -6.9) shouldBe -0.0 +- eps
  }

  "Double.scaleneg" should work in {
    scaleNeg(0.0, 0.0) shouldBe 0.0 +- eps
    scaleNeg(0.0, 6.0) shouldBe 0.0 +- eps
    scaleNeg(0.0, 6.1) shouldBe 0.0 +- eps
    scaleNeg(0.0, 6.5) shouldBe 0.0 +- eps
    scaleNeg(0.0, 6.9) shouldBe 0.0 +- eps
    scaleNeg(0.0, -6.0) shouldBe 0.0 +- eps
    scaleNeg(0.0, -6.1) shouldBe 0.0 +- eps
    scaleNeg(0.0, -6.5) shouldBe 0.0 +- eps
    scaleNeg(0.0, -6.9) shouldBe 0.0 +- eps
    scaleNeg(10.0, 0.0) shouldBe 10.0 +- eps
    scaleNeg(10.0, 6.0) shouldBe 10.0 +- eps
    scaleNeg(10.0, 6.1) shouldBe 10.0 +- eps
    scaleNeg(10.0, 6.5) shouldBe 10.0 +- eps
    scaleNeg(10.0, 6.9) shouldBe 10.0 +- eps
    scaleNeg(10.0, -6.0) shouldBe 10.0 +- eps
    scaleNeg(10.0, -6.1) shouldBe 10.0 +- eps
    scaleNeg(10.0, -6.5) shouldBe 10.0 +- eps
    scaleNeg(10.0, -6.9) shouldBe 10.0 +- eps
    scaleNeg(10.1, 0.0) shouldBe 10.1 +- eps
    scaleNeg(10.1, 6.0) shouldBe 10.1 +- eps
    scaleNeg(10.1, 6.1) shouldBe 10.1 +- eps
    scaleNeg(10.1, 6.5) shouldBe 10.1 +- eps
    scaleNeg(10.1, 6.9) shouldBe 10.1 +- eps
    scaleNeg(10.1, -6.0) shouldBe 10.1 +- eps
    scaleNeg(10.1, -6.1) shouldBe 10.1 +- eps
    scaleNeg(10.1, -6.5) shouldBe 10.1 +- eps
    scaleNeg(10.1, -6.9) shouldBe 10.1 +- eps
    scaleNeg(10.5, 0.0) shouldBe 10.5 +- eps
    scaleNeg(10.5, 6.0) shouldBe 10.5 +- eps
    scaleNeg(10.5, 6.1) shouldBe 10.5 +- eps
    scaleNeg(10.5, 6.5) shouldBe 10.5 +- eps
    scaleNeg(10.5, 6.9) shouldBe 10.5 +- eps
    scaleNeg(10.5, -6.0) shouldBe 10.5 +- eps
    scaleNeg(10.5, -6.1) shouldBe 10.5 +- eps
    scaleNeg(10.5, -6.5) shouldBe 10.5 +- eps
    scaleNeg(10.5, -6.9) shouldBe 10.5 +- eps
    scaleNeg(10.9, 0.0) shouldBe 10.9 +- eps
    scaleNeg(10.9, 6.0) shouldBe 10.9 +- eps
    scaleNeg(10.9, 6.1) shouldBe 10.9 +- eps
    scaleNeg(10.9, 6.5) shouldBe 10.9 +- eps
    scaleNeg(10.9, 6.9) shouldBe 10.9 +- eps
    scaleNeg(10.9, -6.0) shouldBe 10.9 +- eps
    scaleNeg(10.9, -6.1) shouldBe 10.9 +- eps
    scaleNeg(10.9, -6.5) shouldBe 10.9 +- eps
    scaleNeg(10.9, -6.9) shouldBe 10.9 +- eps
    scaleNeg(-10.0, 0.0) shouldBe 0.0 +- eps
    scaleNeg(-10.0, 6.0) shouldBe 60.0 +- eps
    scaleNeg(-10.0, 6.1) shouldBe 61.0 +- eps
    scaleNeg(-10.0, 6.5) shouldBe 65.0 +- eps
    scaleNeg(-10.0, 6.9) shouldBe 69.0 +- eps
    scaleNeg(-10.0, -6.0) shouldBe -60.0 +- eps
    scaleNeg(-10.0, -6.1) shouldBe -61.0 +- eps
    scaleNeg(-10.0, -6.5) shouldBe -65.0 +- eps
    scaleNeg(-10.0, -6.9) shouldBe -69.0 +- eps
    scaleNeg(-10.1, 0.0) shouldBe 0.0 +- eps
    scaleNeg(-10.1, 6.0) shouldBe 60.6 +- eps
    scaleNeg(-10.1, 6.1) shouldBe 61.61 +- eps
    scaleNeg(-10.1, 6.5) shouldBe 65.65 +- eps
    scaleNeg(-10.1, 6.9) shouldBe 69.69 +- eps
    scaleNeg(-10.1, -6.0) shouldBe -60.6 +- eps
    scaleNeg(-10.1, -6.1) shouldBe -61.61 +- eps
    scaleNeg(-10.1, -6.5) shouldBe -65.65 +- eps
    scaleNeg(-10.1, -6.9) shouldBe -69.69 +- eps
    scaleNeg(-10.5, 0.0) shouldBe 0.0 +- eps
    scaleNeg(-10.5, 6.0) shouldBe 63.0 +- eps
    scaleNeg(-10.5, 6.1) shouldBe 64.05 +- eps
    scaleNeg(-10.5, 6.5) shouldBe 68.25 +- eps
    scaleNeg(-10.5, 6.9) shouldBe 72.45 +- eps
    scaleNeg(-10.5, -6.0) shouldBe -63.0 +- eps
    scaleNeg(-10.5, -6.1) shouldBe -64.05 +- eps
    scaleNeg(-10.5, -6.5) shouldBe -68.25 +- eps
    scaleNeg(-10.5, -6.9) shouldBe -72.45 +- eps
    scaleNeg(-10.9, 0.0) shouldBe 0.0 +- eps
    scaleNeg(-10.9, 6.0) shouldBe 65.4 +- eps
    scaleNeg(-10.9, 6.1) shouldBe 66.49 +- eps
    scaleNeg(-10.9, 6.5) shouldBe 70.85 +- eps
    scaleNeg(-10.9, 6.9) shouldBe 75.21 +- eps
    scaleNeg(-10.9, -6.0) shouldBe -65.4 +- eps
    scaleNeg(-10.9, -6.1) shouldBe -66.49 +- eps
    scaleNeg(-10.9, -6.5) shouldBe -70.85 +- eps
    scaleNeg(-10.9, -6.9) shouldBe -75.21 +- eps
  }
}