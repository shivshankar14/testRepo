package AlgorithimPractice

object Practice1 {

  def main(args: Array[String]):Unit = {
    println("Shiv 2314545")

    println(Fibonacci(10))

    println(FibonacciOptimized(10))

    //Calling randomArray function
    val arr = Array(1,2,3,4,5,6,7)
    val outputArr = RandomArray(arr)
    outputArr.foreach(println)
    //List(6, 4, 1, 7, 2, 3, 5)
    println(outputArr.toList)

    val inputArray2 = Array(-7,-6,-3,-2,0,1,2,3,4,5)
    val outputArray = SortedSquare(inputArray2)
    println(outputArray.toList)

    val inputArray3 = Array(2, 3, 4, 10, 40, 45, 59, 60, 76)
    val itemToFind = 60

    val result: Int = FindAnIndexOfAItemInSortedArray(inputArray3,itemToFind, 0, inputArray3.length - 1)
    if(result == -1) println("Item not found in array")
    else println("Item found at Index " + result)

    if(IsPrimeNumber(20)) println("True")
    else println("False")

    val inputArray4 = Array(6, 9, 4, 5, 13, 15, 8, 10, 11, 7, 12)
    val missingNumber: Int = FindMissingNumber(inputArray4,4,15)
    println("Missing Number is " + missingNumber)

    if(IsIntPlaindrome(1412141)) println("This integer is plaindrome")
    else println("Not a plaindrome")

    val outputStr = ReverseWord("D")
    println(outputStr)


    println(ReverseWordOfString("My Name    is Shiv         Shankar Prasad"))

    // Output would be
    // yM emaN si vihS raknahS dasarP

  }

  //Recursive
  def Fibonacci(n:Int): Int = {
    if(n == 0 || n == 1)
      return n
    else
      Fibonacci(n-1) + Fibonacci(n-2)
  }

  def FibonacciOptimized(n: Int): Int = {
    var a: Int = 0
    var b: Int = 1
    var c: Int = 0
    if(n == 0 || n == 1){
      return n
    }
    else {
      for(i: Int <- 2 to n){
        c = a + b
        a = b
        b = c
      }
      return b
    }
  }

  def RandomArray(inputArray: Array[Int]): Array[Int] ={
    for(i:Int <-0 until inputArray.length){
      val temp: Int = inputArray(i)
      val r: Int = scala.util.Random.nextInt(inputArray.length-1)
      inputArray(i) = inputArray(r)
      inputArray(r) = temp
    }
    return inputArray
  }

  // Merge sort time complexity O(n log n)
  def SortedSquare(inputArr: Array[Int]): Array[Int] = {
    val loop = new scala.util.control.Breaks
    var j = 0
    loop.breakable{
      for(i <- 0 until inputArr.length){
        if(inputArr(i) >= 0){
          j=i
          loop.break
        }
      }
    }
    var k = j-1//Last index of negative integers
    var n = j//First index of positives numbers
    var ind = 0 // First index of temp array

    val tempArray = new Array[Int](inputArr.length)
    while (k >= 0 && n < inputArr.length){
      if(inputArr(k)*inputArr(k) < inputArr(n) * inputArr(n)){
        tempArray(ind) = inputArr(k)*inputArr(k)
        k -= 1
      }
      else{
        tempArray(ind) = inputArr(n) * inputArr(n)
        n += 1
      }
      ind += 1
    }
    while (k >= 0){
      tempArray(ind) = inputArr(k)*inputArr(k)
      k -= 1
      ind += 1
    }
    while (n < inputArr.length){
      tempArray(ind) = inputArr(n) * inputArr(n)
      n += 1
      ind += 1
    }
    return tempArray
  }

  //Time Complexity id O(Log n)
  // BinarySearch algorithm

  def FindAnIndexOfAItemInSortedArray(inputArray: Array[Int], itemToFind: Int, startIndex: Int, endIndex: Int): Int = {

    if(endIndex >= startIndex){
      var midIndex = startIndex + (endIndex - startIndex)/2

      if(inputArray(midIndex) == itemToFind){
        return midIndex
      }
      if(inputArray(midIndex) > itemToFind){
        return FindAnIndexOfAItemInSortedArray(inputArray,itemToFind, startIndex, midIndex -1)
      }
      if(inputArray(midIndex) < itemToFind){
        return FindAnIndexOfAItemInSortedArray(inputArray,itemToFind,midIndex +1, endIndex)
      }
    }
    return -1
  }

  def IsPrimeNumber(n: Int): Boolean = {
    if(n <= 1) return false
    for(i: Int <- 2 to n if i*i <= n){
      if(n % i == 0){
        return false
      }
    }
    return true
  }

  def FindMissingNumber(inputArray: Array[Int], m: Int, n: Int): Int = {
    var missingNumber: Int = 0
    val n1: Int = n
    val n2: Int = m - 1
    var sumOfNumbers: Int = ((n1 * (n1 + 1)) / 2) - ((n2 * (n2 + 1)) / 2)
    for (i <- 0 until inputArray.length) {
      sumOfNumbers = sumOfNumbers - inputArray(i)
    }
    missingNumber = sumOfNumbers
    return missingNumber
  }

 def IsIntPlaindrome(num: Int) : Boolean = {
   var remainder: Int = 0
   var reverse: Int = 0
   var temp: Int = num
   if(num > 0 & num < 10) return true
   while(temp > 0)
     {
       remainder = temp % 10
       reverse = reverse * 10 + remainder
       temp = temp / 10
     }
   if(num == reverse){
     return true
   }
   else return false
 }

  //Reverse the word
  def ReverseWord (inputStr: String): String = {
    val charArray: Array[Char] = inputStr.toCharArray()
    var minIndex = 0
    var maxIndex = charArray.length - 1

    while(minIndex < maxIndex){
      val temp: Char = charArray(minIndex)
      charArray(minIndex) = charArray(maxIndex)
      charArray(maxIndex) = temp
      minIndex += 1
      maxIndex -= 1
    }
    return new String(charArray)
  }

  //Revers each word of a sting
  def ReverseWordOfString(inputStr: String): String = {
    //val strArray: Array[String] = inputStr.split(' ')

    val strArray: Array[String] = inputStr.split("\\s+")

    var outputStr: String = ""
    for (i <- 0 until strArray.length) {
      outputStr = outputStr + " " + ReverseWord(strArray(i))
    }
    return outputStr
  }


  }
