-- | The Error tests
--
-- @since 0.1.0
module Error
  ( errorSpec
  ) where

import Control.Lens     ((^.))
import Failure.Error    (code, description, err, nextCause)
import Failure.Fail     (Fail (backtrace, cause, (+>)))
import Test.Tasty.Hspec (Spec, it, parallel, shouldBe, shouldContain)

-- Testable error codes
data Code
  = Error1
  | Error2
  deriving (Eq, Show)

-- Error.hs related unit tests
errorSpec :: Spec
errorSpec =
  parallel $
    -- Given
   do
    let testErrorCode = Error1
    let testErrorDescription = "Error"
    let testError = err testErrorCode testErrorDescription
    let nextTestErrorDescription = "Next Error"
    let nextTestErrorCode = Error2
    let nextTestError =
          testError +> err nextTestErrorCode nextTestErrorDescription
    --
    it "should succeed to create an Error from description" $ do
      show testError `shouldContain` testErrorDescription
      show testError `shouldContain` show testErrorCode
    --
    it "should succeed to retrieve the code of an Error" $
      testError ^. code `shouldBe` testErrorCode
    --
    it "should succeed to retrieve the description of an Error" $
      testError ^. description `shouldBe` testErrorDescription
    --
    it "should succeed to retrieve the next Error if existing" $
      nextTestError ^. nextCause `shouldBe` Just testError
    --
    it "should fail to retrieve the next Error if not existing" $
      testError ^. nextCause `shouldBe` Nothing
    --
    it "should succeed to append an Error to another" $ do
      show nextTestError `shouldContain` testErrorDescription
      show nextTestError `shouldContain` show testErrorCode
      show nextTestError `shouldContain` nextTestErrorDescription
      show nextTestError `shouldContain` show nextTestErrorCode
    --
    it "should succeed to retrieve the cause for an Error with one" $
      cause nextTestError `shouldBe` Just testError
    --
    it "should fail to retrieve the cause for an Error without one" $
      cause testError `shouldBe` Nothing
    --
    it "should succeed to backtrace an Error" $
      backtrace testError `shouldBe` [testError]
    --
    it "should succeed to backtrace two Errors" $
      backtrace nextTestError `shouldBe` [nextTestError, testError]
