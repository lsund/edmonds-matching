// graph.cpp

#include <iostream>
#include <algorithm>

#include <ostream>	// for output streams
#include <istream>	// for input streams
#include <fstream>	// for actual file streams
#include <string>	// necessary for the line parser
#include <sstream>	// necessary for the line parser

#include "graph.hpp"


// Lese aus dem gegebenen std::istream & initialisiere den Graph.

/* Die Instanz ist wie folgt gespeichert:
   Anzahl von Knoten
   Kante1
   Kante2
   ...
   Wichtig: Knoten fangen mit Index 0 an */

int EdmondGraph::read(std::istream & in){
  std::string line;
  // Beim einlesen ist der Algorithmus noch nicht ausgeführt worden.
  solved = false;
  
  // Lese die erste Zeile (Anzahl der Knoten)
  std::getline(in, line);
  // Wenn die Zeile leer ist, return 1.
  if (line.empty()){
    std::cerr << "falsche Formatierung, erste Zeile darf keine leere Zeile sein";
    return 1;
  }
  
  std::istringstream lineStream(line);
  unsigned long number;
  lineStream >> number;
  // Initialisierung von mu, phi, rho, adjacent_structure und scanned
  init(number);
  
  // lese die Kanten ein:
  while (std::getline(in, line)){
    if(line.empty())
      continue;
    
    std::istringstream lineStream(line);
    unsigned long firstParam, secondParam;
    lineStream >> firstParam >> secondParam;
    // Konstruiere die adjacents Struktur
    adjacent_structure[firstParam].push_back(secondParam);
    adjacent_structure[secondParam].push_back(firstParam);
  }
  
  maximalMatching();
  
  return 0;
    
}

// Liest den gegebenen Dateinamen und führt read() aus
// return 2, wenn der File nicht geöffnet werden konnte

int EdmondGraph::load(std::string filename){
  std::ifstream inputFile(filename.c_str());
  if(!inputFile){
    std::cerr << "File konnte nicht gelesen werden";
    return 2;
  }
  
  return read(inputFile);
}

// Ruft den Algorithmus auf und gibt die Lösung aus
void EdmondGraph::solution(std::ostream & out){
  // Wenn der Algorithmus noch nicht ausgeführt wurde, führe ihn aus
  if(!solved)
    run();
  
  // Größe des Matchings in der ersten Zeile
  out << "Die Kardinalitaet des Matchings ist " << matching.size() << "\n";
  // Gebe alle Kanten aus
  for(matching_type::iterator it = matching.begin(); it != matching.end(); ++it)
    out << it->first << " " << it->second << "\n";
  
  // Buffer wird geleert
  out.flush();
}


// TEST: Gibt den Graphen aus
void EdmondGraph::graphoutput(std::ostream & out){
  out << "Anzahl der Knoten" << adjacent_structure.size() << "\n";
  for(unsigned long i=0; i<adjacent_structure.size(); ++i){
    out << "Liste für Knoten " << i << ": ";
    for(adjacent_vertex_list::iterator it=adjacent_structure[i].begin(); it != adjacent_structure[i].end(); ++it){
      out << *it << " ";
    }
    out << "\n";
  }
  out.flush();
}
// TEST: Gibt mu, phi und rho aus
void EdmondGraph::muphirhooutput(std::ostream & out){
  out << "mu, phi, rho \n";
  for(unsigned long i=0; i<adjacent_structure.size(); ++i){
    out << mu[i] << " , " << phi[i] << " ' " << rho[i] << "\n";
  }
}



// Initialisiere den Graph 

// 1. mu, phi, rho, adjacent_structure und den scanned-Vektor.
void  EdmondGraph::init(unsigned long number_vertex){
  // Setze mu, phi, rho und den scanned-Vector auf die richtige Größe
  mu.resize(number_vertex);
  phi.resize(number_vertex);
  rho.resize(number_vertex);
  adjacent_structure.resize(number_vertex);
  
  // Setze mu, phi und rho auf den Index
  for(unsigned long i = 0 ; i<number_vertex ; ++i){
    mu[i] = i;
    phi[i] = i;
    rho[i] = i;
  }
  //Setze alle Knoten auf unscanned
  setAllUnscanned(); 
}

// suche maximales Matching zur Initialisierung:
void EdmondGraph::maximalMatching(){
  for (unsigned long i=0; i<adjacent_structure.size(); ++i){
    if (mu[i]==i){
      // Dann ist i noch in keiner Matchibng Kante
      const adjacent_vertex_list & edges = adjacent_structure[i];
  
      for(adjacent_vertex_list::const_iterator it = edges.begin(); it !=edges.end(); ++it){
	if(mu[*it] == *it ){
	  mu[i]=*it;
	  mu[*it]=i;
	  break;
	}
      }
    }
  }
}


// 2. Finde einen äußeren Knoten, der noch nicht gescannt wurde:
/* erhalte als Rückgabewert den Index eines äußeren ungescannten Knotens 
   oder die Anzahl der Knoten, wenn es keinen solchen Knoten gibt */
EdmondGraph::vertex_index EdmondGraph::getUnscannedOuterVertex() const{
  for(vertex_index i = 0; i < adjacent_structure.size(); ++i){
    if(isOuter(i) && !isScanned(i)) {
      return i;
    }
  }
  return adjacent_structure.size();
}


// 3. Finde einen Nachbarn y von x, sodass y nicht im Wald ist oder y ein äußerer Knoten mit rho(x)!=rho(y)
/* erhalte als Rückgabewert den Index des gewünschten Nachbarn 
   oder die Anzahl der Knoten, wenn es keinen solchen Nachbarn gibt */
EdmondGraph::vertex_index EdmondGraph::getSuitableNeighbor(vertex_index x){
  // die inzidenten Kanten sollen nicht verändert werden
  // Referenz zur besseren Lesbarkeit
  const adjacent_vertex_list & edges = adjacent_structure[x];
  // gehe alle Nachbarn durch, ob einer die gewünschten Eigenschaften hat
  for(adjacent_vertex_list::const_iterator it = edges.begin(); it !=edges.end(); ++it){
    if ( isNotInForest(*it) || (isOuter(*it) && rho[*it]!=rho[x]) )
      return *it;
  }
  return adjacent_structure.size();
}


// 5. & 6. Finde P(x) und P(y), bzw. P(x)_[x,r] und P(y)_[y,r] zu den beiden benachbarten Knoten
/* erhalte als Rückgabewert einen gültigen Index, d.h. den Knoten an dem sich die beiden Pfade 
   schneiden oder einen ungültigen Index (Anzahl der Knoten), d.h. die beiden Pfade schneiden 
   sich nicht.*/
EdmondGraph::vertex_index EdmondGraph::getAlternatingPaths(
    vertex_index x,
    vertex_index y,
    path_type & Px,
    path_type & Py
){
  
  // Vektor, der testet ob ein Knoten schon in einem der Pfade ist.
  std::vector<bool> check(adjacent_structure.size(), false);
  
  // Füge die Startpunkte zu den Pfaden und setze den Knoten auf gecheckt
  Px.push_back(x);
  check[x]=true;
  Py.push_back(y);
  check[y]=true;
  
  // Speicher ob der Pfad schon fertig konstruiert wurde, bzw. ob r gefunden wurde
  bool ended[2] = {false, false};
  
  // Speichert den momentan letzten Knoten der beiden Pfade
  vertex_index current[2] = {x, y};
  
  
  // Der erste Knoten, an dem sich die Pfade treffen, wenn es diesen nicht gibt, setze den Index auf einen ungültigen Index.
  vertex_index r = adjacent_structure.size();
  
  // Solange die Pfade noch nicht zuende sind oder der gemeinsamm Knoten nicht gefunden wurde konstruiere die Pfade weiter
  while(!ended[0] || !ended[1]){
    
    for(unsigned int i=0;i<2; ++i){
      // Wenn der Pfad schon zu ende ist, tue nichts
      if (ended[i])
	continue;
      /* NOTE: der erste gemeinsame Knoten ist ein phi(_) Knoten UND der erste gemeinsame 
        Knoten mit rho(_)=_ muss die Eigenschaft auch haben, da die Wurzel einer Blüte immer 
        ein äußere Knoten ist.
        ABER der ertse gemeinsamme Knoten und der erste mit genwünschter Eigenschaft müssen
        nicht gleich sein*/
      /* Da der Pfad gerade Länge hat und somit sowohl der letzte Knoten als auch der erste 
        gemeinsame Knoten eine äußerer Knoten ist, endet ein Pfad mit Knoten phi(_) und auch der
        erste gemeinsame Knoten ist ein phi(_) Knoten.*/
      vertex_index next = mu[current[i]];
      // Pfad ist zu ende, wenn mu(_)=phi(mu(_)) (oder der gewünschte gemeinsame Knoten gefunden ist)
      if (next == current[i]){
	ended[i] = true;
      // Füge next zur Liste und schaue ob phi(next) gemeinsamer Knoten sein könnte und füge ihn zur Liste
      }else{
	if (i==0){
	  Px.push_back(next);
	}else{ 
	  Py.push_back(next);
	}
	
	check[next]=true;
	current[i]=next;
	next=phi[current[i]];
	
	// next kommt auf jeden Fall zum Pfad dazu. Die Frage ist, ob es der gemeinsamme Knoten ist.
	if (i==0){
	  Px.push_back(next);
	}else{ 
	  Py.push_back(next);
	}
	
	// Wenn der Knoten next schon in einem Pfad ist, dann ist dieser ein Kandidat für r
	if(check[next]==true && rho[next]==next){
	// next ist gemeinsamer Knoten der beiden Pfade
	// Prüfe ob er der gesuchte Knoten ist
	  r = next;
	  ended[0]=true;
	  ended[1]=true;
	  // Lösche den überflüssigen Pfad
	  while(Px.back() != r){
	    Px.pop_back();
	  }
	  while(Py.back() != r){
	    Py.pop_back();
	  }
	
	// Noch kein gemeinsamer Knoten gefunden
	}else{
	  check[next]=true;
	  current[i]=next;
	}
      }
    }
    
  }
  
  return r;
  
}

// 5. augmentieren eines Pfades und augmentieren der beiden Pfade:
// Augmentiere entlang eines Pfades:
void EdmondGraph::augmentSinglePath(path_type & Path){ 
  //Setze für jeden Knoten v mit ungerader Distanz zum Startknoten w von Pfad: mu(phi(v)) = v, mu(v)=phi(v)
  for(path_type::iterator it=(++Path.begin()); it!=Path.end(); it++){
    
    mu[phi[*it]] = *it;
    mu[*it] = phi[*it];
    // Damit zweier Schritte gegangen werden
    if((++it)==Path.end())
      break;
  }
}

// Augmentiere entlang beider Pfade:
void EdmondGraph::augmentPaths(path_type & PathX, path_type & PathY){
  augmentSinglePath(PathX);
  augmentSinglePath(PathY);
}

//6. Schrumpfe "einen Pfad" und schrumpfe beide Pfade:
void EdmondGraph::shrinkSinglePath(path_type& Path, vertex_index r){
  // Setze für jeden Knoten v mit ungerader Distanz zum Startknoten w von Pfad und rho(phi(v)) !=r : phi(phi(v))=v
  for(path_type::iterator it=(++Path.begin()); it!=Path.end(); it++ ){
    
    if (rho[phi[*it]] != r){
      phi[phi[*it]] = *it;
    }
    // Damit zweier Schritte gegangen werden
    if((++it)==Path.end())
      break;
  }
}

// Schrumpfe beide Pfade:
void EdmondGraph::shrinkPaths(path_type & PathX, path_type & PathY, vertex_index r){
  const vertex_index & x = PathX.front();
  const vertex_index & y = PathY.front();
  
  // Schrumpfe die beiden Pfade
  shrinkSinglePath(PathX,r);
  shrinkSinglePath(PathY,r);
  
  if(rho[x]!=r)
    phi[x]=y;
  if(rho[y]!=r)
    phi[y]=x;
  
  // r ist Basis der Blüte -> markiere alle Knoten bei denen rho(v) in einem der beiden Pfade ist
  /* d.h alle äußeren Knoten v bei denen die Basis der v enthaltenen äußeren Blüte in einem der
     beiden Pfade liegt. */
  // Makiere alle Knoten die in einem der beiden Pfade sind
  std::vector<unsigned short> vunion(adjacent_structure.size(),0);
  for(path_type::const_iterator it = PathX.begin(); it != PathX.end(); ++it){
    vunion[*it]++;
  }
  for(path_type::const_iterator it = PathY.begin(); it != PathY.end(); ++it){
    vunion[*it]++;
  }
  // Setze für v, mit rho(v) in den Pfaden: rho(v)=r:
  for(vertex_index v = 0; v < adjacent_structure.size(); ++v){
    // Wenn rho(v) in einem der Pfade ist setze es auf r
    if(vunion[rho[v]])
      rho[v]=r;
  }
  
}


// Konstruiere das Matching aus mu: M={ {x,y} | x!=y and mu(y)=x }
void EdmondGraph::prepareMatching(){
  // Da mu(x)=y und mu(y)=x, wir habe nur jeden Kante einmal im Matching haben wollen, speichern wird die schon verwendeten Knoten
  std::vector<bool> check(adjacent_structure.size(), false);
  matching.clear();
  for(vertex_index i = 0; i < adjacent_structure.size(); ++i){
    if(i != mu[i] && !check[i] && !check[mu[i]]){
      check[i] = check[mu[i]] = true;
      matching.push_back(std::make_pair(i, mu[i]));
    }
  }
}


// Der Algorithmus:
void EdmondGraph::run(){
     
  //Falls der Algorithmus schon einmal ausgeführt wurde, soll er nicht erneut ausgeführt werden
  if(solved)
    return;
  // Zeit Messung: maximalMatching();
  // TEST muphirhooutput(std::cout);
  //So lange es einen äußeren Knoten gibt, der nicht gescannt wurde, läuft der Algorithmus weiter
  // #2. Bekomme einen äußeren Knoten, der nicht gescannt wurde oder beende den Algorithmus
  for( vertex_index x=getUnscannedOuterVertex(); isValidIndex(x); x=getUnscannedOuterVertex() ){
    // TEST std::cout << "Knoten x=" << x << "\n";
    // Ohne goto: komme in die nächste Schleifeniteration -> nächster Knoten x
    bool jumpOut = false;
    // TEST std::cout << "Komme in while Schleife:" << "\n";
    while(!jumpOut){

      // #3. Suche gültigen Nachbarn von x
      vertex_index y = getSuitableNeighbor(x);
      // TEST std::cout << " Knoten y=" << y << "\n";
      // Wenn es keinen gibt, dann setze x auf gescannt und gehe zu #2
      if(!isValidIndex(y)){
	// TEST std::cout << " gibt keinen Nachbarn von" << x << "\n";
	setScanned(x);
	break;
      }
      // Sonst tritt einer der Fälle "anwachsen", "augmentieren" oder "schrumpfen" ein
      // y ist Nachbar von x
      
      // #4. "anwachsen"
      if(isNotInForest(y)){
	// TEST std::cout << " Bin in Fall 1 \n";
	phi[y]=x;
	// TEST muphirhooutput(std::cout);
	// goto #3.
	continue;
      }
      
      // Pfade P(x) und P(y), bzw. P(x)_[x,r] und P(y)_[y,r] müssen konstruiert werden um zu schauen in welchen Fall wir kommen.
      path_type Px, Py;
      vertex_index r = getAlternatingPaths(x,y,Px,Py);
      // TEST std::cout << "gemeinsamme Knoten: " << r << "\n";
      
      // #5. "augmentieren"
      // Wenn die beiden Pfade disjunkt sind und somit r kein zulässiger Index ist.
      if(!isValidIndex(r)){
	// TEST std::cout << " Bin in Fall 2 \n";
	// Augmentiere entlang des Pfades P(x) und P(y)
	augmentPaths(Px,Py);
	//Füge Kante {x,y} zum Matching
	mu[x] = y;
	mu[y] = x;
	// Setze phi und rho wieder "auf Anfang" und alle Knoten auf ungescannt
	for(vertex_index i = 0; i < adjacent_structure.size(); ++i){
	  phi[i] = i;
	  rho[i] = i;
	}
	setAllUnscanned();
	// Gehe zu Schritt 2:
	jumpOut = true;
	//TEST muphirhooutput(std::cout);
      } 
      
      // #6. "schrumpfen"
      else{
	// TEST std::cout << " Bin in Fall 3 \n";
	shrinkPaths(Px,Py,r);
	//TEST muphirhooutput(std::cout);
      }
    }
  }
  
  // speicher das Matching M={ {x,y} | x!=y and mu(x)=y}
  prepareMatching();
  solved = true;
  
}
