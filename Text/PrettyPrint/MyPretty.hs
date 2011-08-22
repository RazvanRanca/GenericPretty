{-| MyPretty is a library that can be used in conjuncture with "Text.PrettyPrint.GenericPretty".
    
    The Pretty library(<http://www.haskell.org/ghc/docs/7.0.4/html/libraries/ghc-7.0.4/Pretty.html>)
    plans to incorporate a 'Style' datatype to control details
    of printing (such as line length).  The library MyPretty is provided as a
    thin wrapper around the Pretty library, to support 'Style' related features.
    Once the Pretty library supports 'Style', MyPretty will become obsolete and
    be replaced by Pretty.
    
    This library can be imported if the user wants to make custom pretty printing definitions for
    his types or define a custom printing 'Style'. 
    The syntax used for defining custom documents is that of Pretty and Text.PrettyPrint.HughesPJ.
    
    For an example of a custom definition for a data type and usage of a custom Style 
    see the README file and the haskell source files under TestSuite, both included in the package.
-}
module Text.PrettyPrint.MyPretty(
                module Pretty,
                Style(..), renderStyle, style
               )where

import Pretty hiding (render)
import FastString

-- | A rendering style
data Style
          = Style {   mode           :: Mode     -- ^ The rendering mode
                    , lineLength     :: Int      -- ^ Length of line, in chars
                    , ribbonsPerLine :: Float    -- ^ Ratio of ribbon length to line length
                  }
                  
-- | Render a document using a particular Style.
renderStyle  :: Style -> Doc -> String
renderStyle s = fullRender (mode s) (lineLength s) (ribbonsPerLine s) outputStr ""
  where
    outputStr :: TextDetails -> String -> String
    outputStr td str = decode td ++ str
      where
        decode :: TextDetails -> String
        decode (PStr s1) = unpackFS s1
        decode (LStr s1 _) = unpackLitString s1
        decode (Chr c)  = [c]
        decode (Str s) = s
        
-- | The default 'Syle'
-- (mode=PageMode, lineLength=80, ribbonsPerLine=1.5)
style :: Style
style = Style {mode = PageMode, lineLength = 80, ribbonsPerLine = 1.5}