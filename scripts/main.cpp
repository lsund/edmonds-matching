#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <time.h>
#include "graph.hpp"

/* Input für die main:
   argc = Anzahl von Argumenten -> muss 2 sein, wobei der 2te ein input file ist
   argv = die Argumente */

int main(int argc, char * argv[]){
  if(argc ==1){
    std::cerr << "Erwarte einen Input File" << std::endl;
    return 1;
  }
  else if(argc >2){
    std::cerr << "ungültiger Wert " << argv[0] << "." << std::endl;
    return 1;
  }
  
  EdmondGraph graph;
  
  //Versuche die datei zu lesen
  if(graph.load(argv[1])){
     std::cerr << "Kann den Graph nicht einlesen vom File " << argv[1] << std::endl;
     return 1;
  }
  // TEST Gebe Graph aus:
  //graph.graphoutput(std::cout);
  
  // Gebe die Lösung in der Konsole aus
  std::stringstream out;
  //Messe die Zeit:
  //clock_t t1 = clock();
  graph.solution(out);
  //clock_t t2 = clock();
  //double t = double(t2-t1)/ CLOCKS_PER_SEC;
  //std::cout << t << std::endl; 
  
  // gibt string zurück
  std::cout << out.str();
  
  return 0;
}