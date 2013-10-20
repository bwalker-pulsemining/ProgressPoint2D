** readme.txt for ProUnit **

1. Using the RunTime version of ProUnit
  1a. You want to run the GUI part
      1. add full path to prounit.pl in your propath (client + AppServer) - AppServer is not mandatory
      2. run startProUnitGUI.p
      3. optionnaly, you can create a shortcut to start a new Progress OpenEdge session from this program

  1b. You want to run the batch part
      1. add full path to prounit.pl in your propath (client + AppServer) - AppServer is not mandatory
      2. run startProUnitBatch.p
      3. optionnaly, you can create a shortcut to start a new Progress OpenEdge session from this program (see online help for parameters)


2. Adding built-in plugins or built-in templates
  Built-in plugins and templates are embedded in .pl file.
  Directory names are hard coded so you just have to update the directory list:
  - plugins:   see pluginsEdit.w in procedure "loadAvailablePlugins"
  - templates: see suiteRunner.p in procedure "getResultTemplates"


3. Troubles when editing ProUnit sources with AppBuilder
  a.
  - Close any Progress window.
  - Re-register prox.dll using "regsvr32.exe /u %DLC%\bin\prox.dll" then "regsvr32.exe %DLC%\bin\prox.dll".
  - Restart your AppBuilder tehn retry.

  b.
  ProUnit uses ActiveX maybe you don't have installed on yout computer.
  To fix this, install VB .net express edition (2008 for instance).
  This will install and register required ActiveX components.



WebSite: http://prounit.sourceforge.net/
  [The ProUnit Team]
