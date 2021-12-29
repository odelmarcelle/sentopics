#include <Rcpp.h>
#include <string>
#include <locale>
#include "convert.h"
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//



// [[Rcpp::export]]
SEXP cpp_minimalFrenchStemmer2(SEXP x) {

  int vec_len = LENGTH(x);
  int len;
  SEXP W;
  char* w;
  // cetype_t enc = Rf_getCharCE(x);

  for (int i = 0; i < vec_len; i++) {
    W = STRING_ELT(x, i);
    w = Rf_acopy_string(Rf_translateCharUTF8(W));
    // Rcout << w << std::endl;
    // len = w.size() - 1;

    // std::string str = "your string in utf8";
    // std::wstring_convert<std::codecvt_utf8_utf16<char16_t>> converter;
    // std::wstring wstr = utf8_to_utf16(w);

    // std::cout << wstr << std::endl;

    Rcout << W << std::endl;
    Rcout << w << std::endl;
    len = LENGTH(W)- 1;
    // for (int j =0; j< len+1; ++j) {
    //   Rcout << w[j] << " and corresponding int " << (int) w[j] << std::endl;
    // }

    if (len > 4) {
      if (w[len]=='x') {
        if (w[len-1]=='u' && w[len-2]=='a') {
          w[len-1]='l';
        }
        w[len]='\0';
      }
      else {
        if ((w[len]=='s') & (len > 4))
        {w[len]='\0';len--;}
        if ((w[len]=='r') & (len > 4))
        {w[len]='\0';len--;}
        if ((w[len]=='e') & (len > 4))
        {w[len]='\0';len--;}
        if ((w[len]== -23) & (len > 4)) // -23 corresponds to 'é', but compiler doesn't like 'é'
        //   // if ( strncmp(w[len], U"\u00E9") == 0 & len > 4)
        //   // if ( strncmp(&w[len], "\x82", 1) == 0 & len > 4)
        {w[len]='\0';len--;}
        if (w[len] == w[len-1])
          w[len]='\0';
      }  /* end else */
    } /* end if (len > 4) */
    if (len > 3) {
      if (w[len]=='s')
      {w[len]='\0';len--;}
    } /* end if (len > 3) */


  W = Rf_mkCharCE(w, CE_UTF8);
  SET_STRING_ELT(x, i, W);

  }


  return(x);
  // static char *  remove_french_plural (word)
  //   char *word;
  // {
  //     int len = strlen (word)-1;
  //
  //     if (len > 4) {
  //       if (word[len]=='x') {
  //         if (word[len-1]=='u' && word[len-2]=='a') {
  //           word[len-1]='l';
  //         }
  //         word[len]='\0';
  //         return(word);
  //       }
  //       else {
  //         if (word[len]=='s')
  //         {word[len]='\0';len--;}
  //         if (word[len]=='r')
  //         {word[len]='\0';len--;}
  //         if (word[len]=='e')
  //         {word[len]='\0';len--;}
  //         if (word[len]=='é')
  //         {word[len]='\0';len--;}
  //         if (word[len] == word[len-1])
  //           word[len]='\0';
  //       }  /* end else */
  //     } /* end if (len > 4) */
  //     return(word);
  // }


}

  // [[Rcpp::export]]
  CharacterVector cpp_minimalFrenchStemmer(CharacterVector x) {

    int vec_len = x.length();
    int len;
    std::string w;

    for (int i = 0; i < vec_len; i++) {
      w = x[i];
      // Rcout << w << std::endl;
      len = w.size() - 1;
      // for (int j =0; j< len+1; ++j) {
      //   Rcout << w[j] << " and corresponding int " << (int) w[j] << std::endl;
      // }

      if (len > 4) {
        if (w[len]=='x') {
          if (w[len-1]=='u' && w[len-2]=='a') {
            w[len-1]='l';
          }
          w[len]='\0';
        }
        else {
          if ((w[len]=='s') & (len > 4))
          {w[len]='\0';len--;}
          if ((w[len]=='r') & (len > 4))
          {w[len]='\0';len--;}
          if ((w[len]=='e') & (len > 4))
          {w[len]='\0';len--;}
          if ((w[len]== -23) & (len > 4)) // -23 corresponds to 'é', but compiler doesn't like 'é'
            //   // if ( strncmp(w[len], U"\u00E9") == 0 & len > 4)
            //   // if ( strncmp(&w[len], "\x82", 1) == 0 & len > 4)
          {w[len]='\0';len--;}
          if (w[len] == w[len-1])
            w[len]='\0';
        }  /* end else */
      } /* end if (len > 4) */
      if (len > 3) {
        if (w[len]=='s')
        {w[len]='\0';len--;}
      } /* end if (len > 3) */
      x[i] = w;
    }


    return(x);
    // static char *  remove_french_plural (word)
    //   char *word;
    // {
    //     int len = strlen (word)-1;
    //
    //     if (len > 4) {
    //       if (word[len]=='x') {
    //         if (word[len-1]=='u' && word[len-2]=='a') {
    //           word[len-1]='l';
    //         }
    //         word[len]='\0';
    //         return(word);
    //       }
    //       else {
    //         if (word[len]=='s')
    //         {word[len]='\0';len--;}
    //         if (word[len]=='r')
    //         {word[len]='\0';len--;}
    //         if (word[len]=='e')
    //         {word[len]='\0';len--;}
    //         if (word[len]=='é')
    //         {word[len]='\0';len--;}
    //         if (word[len] == word[len-1])
    //           word[len]='\0';
    //       }  /* end else */
    //     } /* end if (len > 4) */
    //     return(word);
    // }







}


// [[Rcpp::export]]
CharacterVector cpp_minimalFrenchStemmer3(CharacterVector x) {

  int vec_len = x.length();
  int len;
  std::string tmp;
  std::wstring w;

  for (int i = 0; i < vec_len; i++) {
    tmp = as<std::string>(x[i]);
    w = utf8_to_utf16(tmp);
    // Rcout << w << std::endl;
    len = w.size() - 1;

    if (len > 4) {
      if (w[len]==u'x') {
        if (w[len-1]==u'u' && w[len-2]==u'a') {
          w[len-1]=u'l';
        }
        w[len]='\0';
      }
      else {
        if ((w[len]==u's') & (len > 4))
        {w[len]='\0';len--;}
        if ((w[len]==u'r') & (len > 4))
        {w[len]='\0';len--;}
        if ((w[len]==u'e') & (len > 4))
        {w[len]='\0';len--;}
        if ((w[len]==u'é') & (len > 4)) // -23 corresponds to 'é', but compiler doesn't like 'é'
          //   // if ( strncmp(w[len], U"\u00E9") == 0 & len > 4)
          //   // if ( strncmp(&w[len], "\x82", 1) == 0 & len > 4)
        {w[len]='\0';len--;}
        if (w[len] == w[len-1])
          w[len]='\0';
      }  /* end else */
    } /* end if (len > 4) */
    if (len > 3) {
      if (w[len]==u's')
      {w[len]='\0';len--;}
    } /* end if (len > 3) */
    x[i] = w;
  }


  return(x);

}

/*** R

cpp_minimalFrenchStemmer( c("bananes", "baronnes", "création", "créations", "duplicité"))
minimalFrenchStemmer( c("bananes", "baronnes", "création", "créations", "duplicité"))

cpp_minimalFrenchStemmer2( c("bananes", "baronnes", "création", "créations", "duplicité"))

string <- c("bananes", "baronnes", "création", "créations", "duplicité")
string <- enc2utf8(string)
Encoding(string)
cpp_minimalFrenchStemmer3(string)


*/
