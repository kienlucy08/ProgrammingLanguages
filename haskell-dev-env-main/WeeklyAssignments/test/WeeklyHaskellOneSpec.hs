-- file: Spec.hs
module WeeklyHaskellOneSpec where
import Test.Hspec
import WeeklyHaskellOne

main :: IO ()
main = hspec spec
spec :: Spec
spec = do
  describe "Remove Char" $ do
    it "Remove the Char 'a' from abracadabra is brcdbr" $
        removeChar 'a' "abracadabra" `shouldBe` "brcdbr"
    it "Remove the Char 'a' from an empty string is empty" $
        removeChar 'a' "" `shouldBe` ""
    it "Remove the Char 'b' from illusion is illusion" $
        removeChar 'a' "illusion" `shouldBe` "illusion"
    it "Remove the char '' from illusion is illusion" $
        removeChar  ' ' "illusion" `shouldBe` "illusion"
    it "Remove the char 'b' from bat, should be at" $
        removeChar 'b' "bat" `shouldBe` "at"
    
  describe "Remove Whitespace" $ do
    it "Remove the whitespace from string ' hello it me ' should be helloitme" $
        removeWhitespace " hello it me " `shouldBe` "helloitme"
    it "Remove the whitespace from string ' hello it me \r \t' should be helloitme" $
        removeWhitespace " hello it me \r \t" `shouldBe` "helloitme"
    it "Remove the whitespace from string '\t yes \n' should be helloitme" $
        removeWhitespace "\t yes \n" `shouldBe` "yes"
    it "Remove the whitespace from string '\tyes \r  \n\n' should be helloitme" $
        removeWhitespace "\tyes \r  \n\n" `shouldBe` "yes"
    it "Remove the whitespace from string 'yes \n it is me \t\r' should be helloitme" $
        removeWhitespace "yes \n it is me \t\r" `shouldBe` "yesitisme"  
    it "Remove the whitespace from string 'yes\r' should be yes" $
        removeWhitespace "yes\r" `shouldBe` "yes"  
    it "Remove the whitespace from string '\r\n\t' should be an empty string" $
        removeWhitespace "\r\n\t" `shouldBe` ""
    it "Remove the whitespace from string (carriage return) '\r' should be an empty string" $
        removeWhitespace "\r" `shouldBe` ""
    it "Remove the whitespace from string (new line)'\n' should be an empty string" $
        removeWhitespace "\n" `shouldBe` ""
    it "Remove the whitespace from string (tab)'\t' should be an empty string" $
        removeWhitespace "\t" `shouldBe` ""

  describe "Remove Punctuation" $ do
    it "Remove the punctuation from 'Hello. It's all good, I'm thriving.' should be Hello It's all good I'm thriving" $
        removePunctuation "Hello. It's all good, I'm thriving." `shouldBe` "Hello It's all good I'm thriving"
    it "Remove the punctuation from 'yes it's me' should be yes its me" $
        removePunctuation "yes it's me" `shouldBe` "yes it's me"
    it "Remove the punctuation from 'How can that be. (I don't know what's happening) should be, How can that be I don't know what's happening'" $
        removePunctuation "How can that be. (I don't know what's happening)" `shouldBe` "How can that be I don't know what's happening"
    it "Remove the punctuation from '[yes].' should be, yes" $
        removePunctuation "[yes]." `shouldBe` "yes"
    it "Remove the punctuation from '[].' should be, an empty string" $
        removePunctuation "[]." `shouldBe` ""  
    it "Remove the punctuation from '{yes}.' should be, yes" $
        removePunctuation "{yes}." `shouldBe` "yes"
    it "Remove the punctuation from '{}}.' should be, an empty string" $
        removePunctuation "{}}" `shouldBe` ""  
    it "Remove the punctuation from '(yes)' should be, yes" $
        removePunctuation "(yes)" `shouldBe` "yes"
    it "Remove the punctuation from '()' should be, an empty string" $
        removePunctuation "()" `shouldBe` ""  
    it "Remove the punctuation from ',,.yes' should be, yes" $
        removePunctuation ",,.yes" `shouldBe` "yes"
    it "Remove the punctuation from ',' should be, an empty string" $
        removePunctuation "," `shouldBe` ""  
    it "Remove the punctuation from '....yes....' should be, yes" $
        removePunctuation "....yes...." `shouldBe` "yes"
    it "Remove the punctuation from '.' should be, an empty string" $
        removePunctuation "." `shouldBe` ""  
    

  describe "Chars to ASCII" $ do
    it "Take a string and transfer it into ASCII integer list, 'Hello World!', is [72,101,108,108,111,44,32,119,111,114,108,100,33]" $
        charsToAscii "Hello World!" `shouldBe` [72,101,108,108,111,32,87,111,114,108,100,33]
    it "Take a string and transfer it into ASCII integer list, 'abcde', is [97,98,99,100,101]" $
        charsToAscii "abcde" `shouldBe` [97,98,99,100,101]
    it "Take a string and transfer it into ASCII integer list, '' is [] an empty list" $
        charsToAscii "" `shouldBe` []
    it "Take a string and transfer it into ASCII integer list, '1234567890', is [49,50,51,52,53,54,55,56,57,48]" $
        charsToAscii "1234567890" `shouldBe` [49,50,51,52,53,54,55,56,57,48]
    it "Take a string and transfer it into ASCII integer list, 'a', is [97]" $
        charsToAscii "a" `shouldBe` [97]
    it "Take a string and transfer it into ASCII integer list, 'aBcDeF', is [97,66,99,68,101,70]" $
        charsToAscii "aBcDeF" `shouldBe` [97,66,99,68,101,70]
    it "Take a string and transfer it into ASCII integer list, 'a9Fe00 b45', is [97,57,70,101,48,48,32,98,52,53]" $
        charsToAscii "a9Fe00 b45" `shouldBe` [97,57,70,101,48,48,32,98,52,53]
    it "Take a string and transfer it into ASCII integer list, '[],,,.', is [91,93,44,44,44,46]" $
        charsToAscii "[],,,." `shouldBe` [91,93,44,44,44,46]
    it "Take a string and transfer it into ASCII integer list, '~ ;:', is [126,32,59,58]" $
        charsToAscii "~ ;:" `shouldBe` [126,32,59,58]


  describe "ASCII to chars" $ do
    it "Take an ASCII list and return the characters, [72,101,108,108,111,44,32,119,111,114,108,100,33] is 'Hello World!'" $
        asciiToChars [72,101,108,108,111,32,87,111,114,108,100,33] `shouldBe` "Hello World!"
    it "Take an ASCII list and return the characters, [97,98,99,100,101] is 'abcde'" $
        asciiToChars [97,98,99,100,101]`shouldBe` "abcde" 
    it "Take an ASCII list and return the characters, [] an empty list is ''" $
        asciiToChars [] `shouldBe` ""
    it "Take an ASCII list and return the characters, [49,50,51,52,53,54,55,56,57,48], is '1234567890'" $
        asciiToChars [49,50,51,52,53,54,55,56,57,48] `shouldBe` "1234567890"
    it "Take an ASCII list and return the characters, [97], is 'a'" $
        asciiToChars [97] `shouldBe` "a"
    it "Take an ASCII list and return the characters, [97,66,99,68,101,70] is 'aBcDeF'" $
        asciiToChars [97,66,99,68,101,70] `shouldBe` "aBcDeF"
    it "Take an ASCII list and return the characters, [97,57,70,101,48,48,32,98,52,53], is 'a9Fe00 b45'" $
        asciiToChars [97,57,70,101,48,48,32,98,52,53]`shouldBe` "a9Fe00 b45" 
    it "Take an ASCII list and return the characters, [91,93,44,44,44,46] is'[],,,.'" $
        asciiToChars [91,93,44,44,44,46] `shouldBe` "[],,,."
    it "Take an ASCII list and return the characters, [126,32,59,58] is '~ ;:'" $
        asciiToChars [126,32,59,58] `shouldBe` "~ ;:"

  describe "Shift Ints" $ do
    it "Shift the ints in a list, [1,2,3,4,5,6] by 4 -> [5,6,7,8,9,10]" $
        shiftInts 4 [1,2,3,4,5,6] `shouldBe` [5,6,7,8,9,10]
    it "Shift the ints in a list, [] by 4 -> []" $
        shiftInts 4 [] `shouldBe` []
    it "Shift the ints in a list, [0,0,0,0] by 101 -> [101,101,101,101]" $
        shiftInts 101 [0,0,0,0] `shouldBe` [101,101,101,101]
    it "Shift the ints in a list, [2,3,4,2] by -1 -> [1,2,3,1]" $
        shiftInts (-1) [2,3,4,2] `shouldBe` [1,2,3,1]
    it "Shift the ints in a list, [1,2,3,4,5,6] by 0 -> [1,2,3,4,5,6]" $
        shiftInts 0 [1,2,3,4,5,6] `shouldBe` [1,2,3,4,5,6]
    it "Shift the ints in a list, [] by 0 -> []" $
        shiftInts 0 [] `shouldBe` []
    it "Shift the ints in a list, [1,2,3,4,5,6] by -55 -> [-54,-53,-52,-51,-50,-49]" $
        shiftInts (-5) [1,2,3,4,5,6] `shouldBe` [124,125,126,127,0,1]
    it "Shift the ints in a list, [20,34,54,23] by 10 -> [30,44,64,33]" $
        shiftInts 10 [20,34,54,23] `shouldBe` [30,44,64,33]


  describe "Shift Message" $ do
    it "shifts the message 'hello' by 3 -> 'khoor'" $
        shiftMessage 3 "hello" `shouldBe` "khoor"
    it "shifts the message 'xyz' by -5 -> 'stu'" $
        shiftMessage (-5) "xyz" `shouldBe` "stu"
    it "shifts the message 'The quick brown fox jumps over the lazy dog' by 13 -> 'aur-~\STXvpx-o\DEL|\EOT{-s|\ENQ-w\STXz}\NUL-|\ETXr\DEL-\SOHur-yn\a\ACK-q|t'" $
        shiftMessage 13 "The quick brown fox jumps over the lazy dog" `shouldBe` "aur-~\STXvpx-o\DEL|\EOT{-s|\ENQ-w\STXz}\NUL-|\ETXr\DEL-\SOHur-yn\a\ACK-q|t"
    it "shifts the message '0123456789' by 10 -> ':;<=>?@ABC'" $
        shiftMessage 10 "0123456789" `shouldBe` ":;<=>?@ABC"
    it "shifts the message 'hello' by 0 -> 'hello'" $
        shiftMessage 0 "hello" `shouldBe` "hello"
    it "shifts the message 'love142all101' by 6 -> 'ru|k7:8grr767'" $
        shiftMessage 6 "love142all101" `shouldBe` "ru|k7:8grr767"
    it "shifts the message 'tell me more!' by 16 -> '\EOTu||0}u0}\DEL\STXu1'" $
        shiftMessage 16 "tell me more!" `shouldBe` "\EOTu||0}u0}\DEL\STXu1"
    it "shifts the messsafe '[]//231' by 3 -> '^`22564'" $
        shiftMessage 3 "[]//231" `shouldBe` "^`22564"

