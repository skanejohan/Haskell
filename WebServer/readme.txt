Setting up
==========
cd ~/Others/Haskell/WebServer
sudo /root/.cabal/bin/cabal sandbox init
(create .hs and .cabal files)
sudo /root/.cabal/bin/cabal install scotty --force-reinstalls

Building and running
====================
sudo /root/.cabal/bin/cabal install WebServer.cabal
sudo cp Differentiation.html .cabal-sandbox/bin
sudo .cabal-sandbox/bin/WebServer

Uploading to FTP server
=======================
cd .cabal-sandbox/bin
ftp -p ftpcluster.loopia.se
cd /apps.johanahlgren.se/public_html/mystuff
send WebServer
send Differentiation.html

Download to the VPS
===================
Log in (putty)
wget apps.johanahlgren.se/mystuff/WebServer
wget apps.johanahlgren.se/mystuff/Differentiation.html
chmod a+x WebServer

Start the web server on the VPS
===============================
screen -S WebServer
WebServer/WebServer
CTRL+A CTRL+D

Stoppa på VPS:en
----------------
scrren -ls
screen -r [PID]
%1
CTRL+C

Stop the Apache web server on the VPS (if needed)
=================================================
service apache2 stop
