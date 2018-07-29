package numberTranslator

object NumberTranslator {
  val maxNumberOfDigits = 9
  
  val singleDigits = Map(("0" -> "zero"), ("1" -> "one"), ("2" -> "two"), ("3" -> "three"), 
      ("4" -> "four"), ("5" -> "five"), ("6" -> "six"), ("7" -> "seven"), ("8" -> "eight"), 
      ("9" -> "nine"));
  
  val doubleDigitsUpToTwenty = Map(("10" -> "ten"), ("11" -> "eleven"), ("12" -> "twelve"), ("13" -> "thirteen"), 
      ("14" -> "fourteen"), ("15" -> "fifteen"), ("16" -> "sixteen"), ("17" -> "seventeen"), ("18" -> "eighteen"), 
      ("19" -> "nineteen"));
 
  val doubleDigitsMoreThanTwentyInTens = Map(("2" -> "twenty"), ("3" -> "thirty"), 
      ("4" -> "forty"), ("5" -> "fifty"), ("6" -> "sixty"), ("7" -> "seventy"), ("8" -> "eighty"), 
      ("9" -> "ninety"));
  
  val hundredString = "hundred"
  val thousandString = "thousand"
  val millionString = "million"
  
  val separator = ","
  
  private def translateSingleDigit(oneDigit : String): String = {
    singleDigits(oneDigit)
  }
  
  private def translateDoubleDigit(twoDigits : String): List[String] = {
    val parsedInt = twoDigits.toInt
    val firstDigitString = twoDigits.head.toString
    val secondDigitString =  twoDigits.last.toString
    
    if (firstDigitString == "0" && secondDigitString == "0") List()
    else if (firstDigitString == "0") List(translateSingleDigit(secondDigitString))
    else if (secondDigitString == "0" && parsedInt>=20) List(doubleDigitsMoreThanTwentyInTens(firstDigitString))
    else if (parsedInt >= 20) List(doubleDigitsMoreThanTwentyInTens(firstDigitString), translateSingleDigit(secondDigitString))
    else List(doubleDigitsUpToTwenty(twoDigits))
  }
  
  private def translateTripleDigit(threeDigits : String, millionOrThousand : String = "", 
      separator : String = ""): List[String] = {
    val firstDigitString = threeDigits.head.toString
    
    val result = if (firstDigitString == "0") translateDoubleDigit(threeDigits.tail)
    else if (threeDigits.tail == "00") List(translateSingleDigit(firstDigitString), hundredString)
    else List(translateSingleDigit(firstDigitString), hundredString, "and") ++ translateDoubleDigit(threeDigits.tail)
            
    if (!result.isEmpty) result ++ List(millionOrThousand ++ separator) else result
  }
  
  private def hasOnlyZeros(number: String): Boolean = number.filter(_ != '0').isEmpty
  
  private def validateInput(number: String): Unit = {
    val len = number.length
    if(!number.matches("[0-9]*")) throw new IllegalArgumentException("Input is not a valid number!")
    else if(len > maxNumberOfDigits || len < 1) throw new IllegalArgumentException("Input length should be 1 to maxNumberOfDigits digits long!")
  }
  
  private def removePossibleTrailingSeperator(output: String): String = {
    if (output.last.toString == separator) output.dropRight(1) else output
  }
  
  private def processNonZeroNumber(number: String): String = {
    // Prepend zeros to always have a number with maxNumberOfDigits digits
    val zeroPadding = List.fill(maxNumberOfDigits - number.length)("0").mkString("")
    val paddedNumber = zeroPadding + number
    
    val wordsList = paddedNumber.grouped(maxNumberOfDigits/3).toList.map(_.mkString(""))
    
    val output = (translateTripleDigit(wordsList(0), millionString, separator) ++  
      translateTripleDigit(wordsList(1), thousandString, separator) ++
      translateTripleDigit(wordsList(2))).mkString(" ").trim
      
    removePossibleTrailingSeperator(output)
  }
  
  def translateNumber(number: String): String = {
    validateInput(number)
    
    if(hasOnlyZeros(number)) singleDigits("0") else processNonZeroNumber(number)
  }
}