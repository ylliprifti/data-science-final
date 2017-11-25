#include <iostream>
#include <string>
#include <fstream>
#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
  int readTotaLines(std::string file){
    std::ifstream infile(file);
    std::string temp;
    int count = 0;
    while(std::getline(infile, temp)){
      count++;
    }
    return count;
  }

  // [[Rcpp::export]]
  std::list<std::string> readRandom(std::string file, NumericVector lines) {
    
    std::ifstream infile(file);
    std::string line;
    int size = lines.length();
    
    std::list<std::string> result;
    
    int i = 0;
    int j = 0;
    int current = lines[j];
    while(std::getline(infile, line)){
      
     if (j>=size) 
        break;   //read all lines, Return
      
      current = lines[j];
      
      if(i==current)
        result.push_back(line);// + " END OF LINE \n"; //current lines is returned
      
      if (i>current)          //move next
        j++;
      
      i++;
      
    }
    
    return result;

  }
