// graph.hpp

#ifndef GRAPH_HPP__
#define GRAPH_HPP__
#include <iosfwd>   // include nur Deklarationen
#include <vector>
#include <list>

// Basis Struktur - ungerichteter Graph 

// struct, da alles public sein soll
struct UndirectedGraph{
  
  // Jeder Knoten ist eindeutig über seinen Index identifizierbar
  typedef unsigned long vertex_index;
  
  // Speicher die adjazenten Knoten in einer Liste
  typedef std::list<vertex_index> adjacent_vertex_list;
  
  // Enthält den ungerichtete Graph G=(V,E)
  std::vector<adjacent_vertex_list> adjacent_structure;
  
};


// Basis Struktur einer Instanz für Endmonds's Matching Algorithmus.

// class, da viel private sein soll. 
// Nutze protected - UngerichteterGraph ist public für EndmondGraph und sonst private

class EdmondGraph : protected UndirectedGraph{
  
public:
  // Liest den Input
  int read(std::istream&);
  // Lädt den Input
  int load(std::string);
  // Gibt die Lösung aus
  void solution(std::ostream&);
  //TEST Gibt den Graph aus
  void graphoutput(std::ostream&);
  // TEST Gibt mu, phi und rho aus
  void muphirhooutput(std::ostream&);
  
  // Löst das Problem
  void run();
  
private:
  
  // Ein Pfad ist eine Folge von Knoten
  typedef adjacent_vertex_list path_type;
  
  // Eine Kante ist ein Paar von Knoten
  typedef std::pair<vertex_index, vertex_index> edge_type;
  /* Matching ist eine Menge von Kanten 
   -> Nutze Liste, da nicht klar ist, wie viele Kanten im Matching sind. */
  typedef std::list<edge_type> matching_type;
  
  matching_type matching;	// Matching - Lösung des Algorithmus
  
  bool solved; 			// Indikator, ob Algorithmus bereits ausgeführt wurde
    
  std::vector<vertex_index> mu;	   // mu von Edmonds' Algorithmus
  std::vector<vertex_index> phi;   // phi von Edmonds' Algorithmus
  std::vector<vertex_index> rho;   // rho von Edmonds' Algorithmus
  std::vector<bool> scanned;       // Enthält die Information ob ein Knoten schon gescannten wurde
  
  
  // Funktion zum initialisieren von mu, phi, rho, adjacent_structure und scanned
  void init(unsigned long number_vertex);
  
  // erhalte Nachbarn, der die Bedingung in 3 erfüllt
  /* Finde einen Nachbarn y vom äußeren Knoten vertex_index, wobei y Ein Konten außerhalb des 
     Waldes ist oder (ein äußerer Knoten mit rho(vertex_index) != rho(y)) */
  vertex_index getSuitableNeighbor(vertex_index);
  
  //Finde alternierende Pfade (entweder bis zur Wurzel oder bis zum ersten gemeinsamen Knoten)
  vertex_index getAlternatingPaths(vertex_index, vertex_index, path_type&, path_type&);
  
  // Abfrage ob Knoten x zu einem bestimmten "Type" gehört
  // für bessere Lesbarkeit benutze isNotInForest & is isInForest
  bool isNotInForest(vertex_index x) const;
  bool isInForest(vertex_index x) const;
  bool isInner(vertex_index x) const;
  bool isOuter(vertex_index x) const;
  bool isValidVertex(vertex_index x) const;
  bool isValidIndex(vertex_index x) const;
  
  // Finde einen äußeren Knoten x der noch nicht gescannt wurde
  // -> Wenn es keinen gibt wird die Anzahl der Knoten zurückgegeben
  vertex_index getUnscannedOuterVertex() const;
  
  // Funktionen für Fall 2 (augmentieren) und 3 (schrumpfen)
  void augmentSinglePath(path_type&);
  void shrinkSinglePath(path_type&, vertex_index);
  void augmentPaths(path_type&, path_type&);
  void shrinkPaths(path_type&, path_type&, vertex_index);
  
  // Konstruiere das Matching aus mu
  void prepareMatching();
  
  //Bestimme maximales Matching zur Initialisierung
  void maximalMatching();
  
  // Funktionen für den scanned Vektor
  bool isScanned(vertex_index x) const;
  void setScanned(vertex_index x);
  void setAllUnscanned();

  
};

//Gibt zurück ob x nicht im speziellen Blütenwald ist.
inline bool EdmondGraph::isNotInForest(vertex_index x) const{
    return ( mu[x] != x && phi[x] == x && phi[mu[x]] == mu[x] );
}

//Gibt zurück ob x im speziellen Blütenwald ist.
inline bool EdmondGraph::isInForest(vertex_index x) const{
    return !isNotInForest(x);
}

//Gibt zurück ob x innere Konten im speziellen Blütenwald ist.
inline bool EdmondGraph::isInner(vertex_index x) const{
    return ( phi[mu[x]] == mu[x] && phi[x] != x );
}
//Gibt zurück ob x äußerer Knoten im speziellen Blütenwald ist.
inline bool EdmondGraph::isOuter(vertex_index x) const{
    return ( (mu[x] == x) || (phi[mu[x]] != mu[x]) );
}
//Gibt zurück ob x ein Knoten ist (d.h. der Index im richtigen Bereich liegt)
inline bool EdmondGraph::isValidVertex(vertex_index x) const{
    return x < adjacent_structure.size();
}
//Gibt zurück ob x ein zulässiger Index ist (d.h. der Index im richtgen Bereich liegt)
inline bool EdmondGraph::isValidIndex(vertex_index x) const{
    return isValidVertex(x);
}
// Gibt wieder ob x schon gescannt ist -> gibt wahr wieder, wenn x schon gescannt ist.
inline bool EdmondGraph::isScanned(vertex_index x) const{
    return scanned[x];
}
// markiert Knoten x als gescannt
inline void EdmondGraph::setScanned(vertex_index x){
    scanned[x] = true;
}
//Setze alle Knoten auf ungescannt
inline void EdmondGraph::setAllUnscanned(){
    scanned = std::vector<bool> (adjacent_structure.size(),false);
}


#endif // GRAPH_H