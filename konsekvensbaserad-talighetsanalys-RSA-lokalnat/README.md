**Konsekvensbaserad tålighetsanalys för RSA i lokalnät**



Detta projekt innehåller det R-skript som utvecklades inom ramen för examensarbetet:



"*Konsekvensbaserad tålighetsanalys för RSA i lokalnät: Metodutveckling och implementering av ett R-skript*"

Umeå universitet, 2026.



Skriptet implementerar en konsekvensbaserad analysmetod för uppskattning av tålighet och redundans som stöd till RSA-arbete för lokalnät. Metodval, antaganden och tolkning av resultaten beskrivs i detalj i rapporten.



**Innehåll och struktur**



* "data/"		- Tom mapp där indata matas in
* 1\. "main.R" 		- Huvudfil som styr körningen av analysen
* "R/"			- Mapp med funktionsfiler som anropas av main

  * 2\. "R/settings.R"		  - Inställningar och körflaggor
  * 3\. "R/foundation.R"	  - Grundläggande hjälpfunktioner
  * 4\. "R/build\_topology.R"	  - Uppbyggnad av nättopologi
  * 5\. "R/reduce\_topology.R"	  - Reducering av topologin
  * 6\. "R/segment\_lengths.R"	  - Beräkning av segmentlängder
  * 7\. "R/flow\_graph.R"	  - Flödesgrafer och nåbarhetsanalys
  * 8\. "R/run\_scenarios.R"	  - Generering och körning av scenarier
  * 9\. "R/scenario\_topology.R" - Ombyggnad av scenariotopologi
  * 10\. "R/resupply.R"	  - Alternativa matningsvägar
  * 11\. "R/plots.R"		  - Visualisering av grundnät och scenarier
  * 12\. "R/export.R"		  - Beräkning och export av resultat till Excel



**Användning**



Skriptet är tänkt att användas med stöd av den metodbeskrivning som återfinns i examensarbetet. Tänk på att resultatet från körningarna förväntas innehålla känslig information och bör därför inte spridas.



För normal användning behöver endast inställningar ändras i filen R/settings.R. Därefter sourcas main.R för att köra skriptet.



**Övrigt**



Kodkommentarer är skrivna på svenska, medan variabelnamn och formler är på engelska. Skriptet är framtaget som en del av ett examensarbete och anpassat efter den data som fanns tillgänglig. Vid avvikande struktur på indata kan därför vissa delar av koden behöva anpassas.

