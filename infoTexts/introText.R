# In introText.R
introText <- HTML("
<h4>Anleitung</h4>
<p>Hier einige Tipps, um die App effektiv zu nutzen:</p>
<ol>
  <li>Beginnen Sie damit, oben links eine <strong>Legislaturperiode auszuwählen</strong>. Es stehen vier Optionen zur Verfügung.</li>
  <li>Im ersten Tab <strong><em>Infos zur Legislatur</em></strong> lassen sich einige allgemeine Informationen finden, wie z.B. eine Tabelle, die die Anzahl der Gesetze pro Themenbereich enthält. Außerdem ein politisches Koordinatensystem der Abgeordneten.</li>
  <li>Der Tab <strong><em>Infos zur Abstimmung</em></strong> widmet sich einer konkret ausgewählten Abstimmung im Bundestag. Diese kann links ausgewählt und mit dem Suchfeld verfeinert werden.</li>
  <li>Innerhalb dieses Info-Tabs kann dann das <strong>Abstimmungsverhalten</strong> genauer betrachtet werden, wie z.B. das Abstimmungsverhalten der Fraktionen oder eine regionale Verteilung über Deutschland.</li>
  <li>Der <strong><em>Analyse</em></strong>-Tab kann unabhängig von einer konkreten Abstimmung betrachtet werden. Hier findet das Clustering innerhalb einer gewählten Legislatur statt. Ziel ist es, mithilfe der Auswahlmöglichkeiten links sinnvolle Cluster zu identifizieren.</li>
</ol>
<p>Achtung: Der Analyse Teil braucht ggf. mehr Rechenzeit, wenn zwischen den Daten oder der Clusteranzahl gewechselt wird. </p>
<p>Aktuelles Feedback und dessen Umsetzungsstand ist <a href='https://docs.google.com/spreadsheets/d/1jy6AsNn5nPMMuD6fSeVRstj05TJU3U946OfEOJS3PBk/edit?usp=sharing' target='_blank'>hier</a> einsehbar. </p>
<p>Für weitere Informationen, Fragen und Feedback freue ich mich über eine E-Mail: <a href='mailto:j.bruene@stud.uni-goettingen.de'>j.bruene@stud.uni-goettingen.de</a></p>
")

save(introText, file = "introText.RData")
