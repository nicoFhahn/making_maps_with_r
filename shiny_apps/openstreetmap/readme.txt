WICHTIG: vor beginn die files aus road_network_zipped in road_network extrahieren. 
AUCH WICHTIG: in road_network darf keine ordner struktur entstehen. alles in den root ordner


download und analyses von openstreetmap daten
tab 1 des panel:
	download von daten von osm mit osmdata package
	man kann entweder daten für eine oder mehrere städte downloaden
	alternativ kann man einen oder mehrere standorte auswählen (siehe test files) und
	in radius um standort oder drivetime downloaden
	drivetime = wie weit komm ich in so und so viel minuten von ort weg. da drivetime mit osrm
	demo server berechnet wird, crasht das sehr oft, also wenn dann radius verwenden.
daten können gedownloadet werden. oft sind es über 100 spalten mit sehr sehr vielen NA. man kann switch umlegen
um nur relevant daten zu downloaden, also spalten wo mindestens 5% nicht NA sind.
außerdem sollte man immer value und key auswählen, da sonst download lange dauern kann.
nachdem die daten dargestellt wurden können in combobox alle daten mit bestimmten namen ausgewählt und angezeigt werden
(jeder edeka z.b.)

tab 2:
	download und analyse von daten.
	wieder daten für städte oder umkreis von punkt downloaden.
	analyse mit drei Tools:
		clustering mit dbscan: man kann auswählen wieviel punkte mindestens in einem cluster sind
		und wie weit diese maximal von einander entfernt sein dürfen. epsilon kann leider nicht
		interpretiert werden (da r version von dbscan benutzt wird). punkte werden basierend auf clustergröße 
		eingefärbt
		
		heatmap: Heatmap auf Karte plotten

		getis-ord: statistik die angibt ob ein punkt ein hot- order cold-sport ist. also ob sich viele punkte
		der selben art in der nähe um diesen befinden. die analyse kann mit der auswahl weiterer relevanter gebäude
		in einem bestimmten umkreis ausgeweitert werden.

	außerdem besteht hier die möglichkeit sich das straßennetzwerk mit anzeigen zu lassen (falls ort in deutschland ist), dauert dann kurz.
	würde ich nur verwenden wenn eine stadt ausgewählt ist.
	außerdem die anzahl der einwohner nach plz kann als polygon data hinzugefügt werden (census 2011 oder so um den dreh rum)
	
am ende von dem panel is n link zur osm wiki für mehre infos über key-value paare