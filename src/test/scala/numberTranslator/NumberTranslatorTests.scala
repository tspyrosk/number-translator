package numberTranslator

import org.scalatest.{FlatSpec, Matchers}
import NumberTranslator.translateNumber

class NumberTranslatorTests extends FlatSpec with Matchers{

   "NumberTranslator" should "throw an exception on a non-number input" in {
      intercept[IllegalArgumentException] (translateNumber("74abc"))
   }
  
   it should "throw an exception on an input longer than 9 digits" in {
      intercept[IllegalArgumentException] (translateNumber("1000000000"))
   }
  
   it should "throw an exception on an empty input" in {
      intercept[IllegalArgumentException] (translateNumber(""))
   }

   it should "correctly process 1-digit numbers" in {
    
      translateNumber("0") shouldBe "zero"
      translateNumber("5") shouldBe "five"
      translateNumber("9") shouldBe "nine"
   }
   
   it should "correctly process 2-digit numbers" in {
    
      translateNumber("11") shouldBe "eleven"
      translateNumber("30") shouldBe "thirty"
      translateNumber("99") shouldBe "ninety nine"
   }
   
   it should "correctly process 3-digit numbers" in {
    
      translateNumber("100") shouldBe "one hundred"
      translateNumber("123") shouldBe "one hundred and twenty three"
      translateNumber("504") shouldBe "five hundred and four"
      translateNumber("520") shouldBe "five hundred and twenty"
   }
   
   it should "correctly process numbers in the thousands" in {
    
      translateNumber("1000") shouldBe "one thousand"
      translateNumber("10000") shouldBe "ten thousand"
      translateNumber("100000") shouldBe "one hundred thousand"
      translateNumber("999999") shouldBe "nine hundred and ninety nine thousand, nine hundred and ninety nine"
   }
       
   it should "correctly process numbers in the millions" in {
      translateNumber("1000000") shouldBe "one million"
      translateNumber("56945781") shouldBe "fifty six million, nine hundred and forty five thousand, seven hundred and eighty one"
   }
}