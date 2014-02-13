'use strict';



/********************************************************************************/
/******** "Global" Annotation Table *********************************************/
/********************************************************************************/

/*@ type Annot1 = { ident : string
                  , ann   : string
                  , row   : int
                  , col   : int  
                  , size  : int
                  } */ 

/*@ type Annot  = array [array [annotJ]] */

/*@ annotTable :: Annot */

// var annotTable 
//    = { 5 : { 14 : { ident : "foo"
//                   , ann   : "int -> int"
//                   , row   : 5
//                   , col   : 14
//                   }
//            }
//      , 9 : { 22 : { ident : "map" 
//                   , ann   : "(a -> b) -> [a] -> [b]"
//                   , row   : 9
//                   , col   : 22
//                   }
//            , 28 : { ident : "xs"
//                   , ann   : "[b]" 
//                   , row   : 9 
//                   , col   : 28
//                   }
//            } 
//      }

var annotTable 
   = { 5 : { 14 : { ann : "int -> int" }
           }
     , 9 : { 22 : { ann : "(a -> b) -> [a] -> [b]" }
           , 26 : { ann : "(a -> b)" }
           , 28 : { ann : "[b]" }
           } 
     }

/********************************************************************************/
/******** Function Returning Annot for A Row/Column *****************************/
/********************************************************************************/

var zooper     = "   Int\n-> Bool\n-> IO String";

function getAnnotText(row, col, annT) {
  var rowA = annT[row];
  
  if (!rowA){
    // No annotations defined for this row...
    return null;
  }

  for (var c in rowA){
    if (c == col) {
      // Found annotation beginning at exact row, col
      return rowA[c].ann;
    }
  }
  return null;
}

function annotFun(row, col){
  return getAnnotText(row + 1, col + 1, annotTable);
}

// rkc
function clearAnnotations() { annotTable = {}; }

// rkc
function addAnnot(row, col, ann) {
  if (!annotTable[row]) { annotTable[row] = {}; }
  if (!annotTable[row][col]) { annotTable[row][col] = {}; }
  annotTable[row][col].ann = ann;
}
  


/********************************************************************************/
/******** Set up the Editor *****************************************************/
/********************************************************************************/

var editor = ace.edit("editor");
//editor.setTheme("ace/theme/chaos");
editor.setTheme("ace/theme/clouds");
editor.setReadOnly(true);

//var SrcMode = require("ace/mode/haskell").Mode;
var SrcMode = require("ace/mode/javascript").Mode;
editor.getSession().setMode(new SrcMode());
var typeTooltip = new TokenTooltip(editor, annotFun);

/********************************************************************************/
/******** Using Angular-Binding To Show Annotations (Deprecated) ****************/
/********************************************************************************/

function updateAnnotValue($scope, newVal){
  $scope.annotValue = newVal;
}

/********************************************************************************/
/******** Angular Controller (Deprecated) ***************************************/
/********************************************************************************/

function AnnotDemoCtrl($scope, $http, $location){
   
  $scope.annotValue = "Zogbert Friggleby";
 
  //editor.on("click", function(ev){
  editor.on("mousemove", function(ev){
    $scope.$apply(function () {
      var pos    = ev.getDocumentPosition();
      var tok    = editor.session.getTokenAt(pos.row, pos.column);
      var rng    = editor.session.getAWordRange(pos.row, pos.column);
      var wrd1   = editor.session.getTextRange(rng);
      var line   = editor.session.getLine(pos.row);
      var locRng = localWordRange(line, pos.row, pos.column); 
      var wrd2   = editor.session.getTextRange(locRng);


      if (tok){
         var fooBar = "Row = "   + pos.row    + 
                      "Col "     + pos.column + 
                      "token = " + tok.value  + 
                      "word1 = " + wrd1      +
                      "word2 = " + wrd2
         ;
         updateAnnotValue($scope, fooBar);
      }
    });
  });
} 

