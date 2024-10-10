# legislatureInfoText.R contains the text for the introduction of the legislature tab

legislatureInfoText <- HTML("
<h3>Bundestag </h3>

  <p>Hier finden Sie einige allgemeine Infos zur ausgewählten Legislatur</p>
  <ol>
  <li>Eine Tabelle über die Anzahl der Gesetze pro Thema</li>
  <li>Ein Graph mit der Gesetzesanzahl pro Themenbereich sowie der Quote an fehlenden Abgeordnenten</li>
  <li>Im nächsten Tab ein Politisches Koordinatensystem</li>
  </ol>
  "
)

# Erweiterte Beispielinformationen für jede Legislaturperiode
legislatureInfo <- list(
  "1" = list(
    jahre = "2021-2025",
    text = "<p>Diese Legislaturperiode umfasst die Jahre 2021 bis 2025. In dieser Zeit wurden wichtige Gesetze in Bereichen wie Klimaschutz, Digitalisierung, und soziale Gerechtigkeit erlassen. Die Bundesregierung hat auch Maßnahmen zur Bewältigung der COVID-19-Pandemie und zur Förderung nachhaltiger Entwicklung eingeführt.</p>",
    abgeordnete = 736,  
    parteien = 9,      
    koalitionen = "Ampelkoalition zwischen SPD, Grünen und FDP"
  ),
  "2" = list(
    jahre = "2017-2021",
    text = "<p>Die Legislaturperiode von 2017 bis 2021 war geprägt von politischen Debatten über Migration, Wirtschaftspolitik und Klimawandel.</p>",
    abgeordnete = 709,
    parteien = 6,
    koalitionen = "Große Koalition zwischen CDU/CSU und SPD"
  ),
  "3" = list(
    jahre = "2013-2017",
    text = "<p>Von 2013 bis 2017 fokussierte sich der Bundestag stark auf die Eurokrise, Energiepolitik und die Förderung erneuerbarer Energien.</p>",
    abgeordnete = 631,
    parteien = 5,
    koalitionen = "Schwarz-Rote Koalition"
  ),
  "4" = list(
    jahre = "2009-2013",
    text = "<p>Von 2009 bis 2013 erlebte Deutschland signifikante politische Entscheidungen, einschließlich Maßnahmen zur Bekämpfung der Finanzkrise und zur Stärkung der Wirtschaft.</p>",
    abgeordnete = 622,
    parteien = 5,
    koalitionen = "Schwarz-Gelbe Koalition zwischen CDU/CSU und FDP"
  )
)



