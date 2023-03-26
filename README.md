# About  
It is a universal GUI, allowing you to open an input file, pass it to any console application with the parameters set inside and specify the output file.  
It can be used as audio converter, for example. Or any other.  
This application was written by Roland57 from [MLO forum](https://www.mageialinux-online.org/forum/topic-30422+universal-gui-input-output.php). There you can read the discussion in French.  
[Roland's project](https://github.com/rchastain/ui) may have new changes

# Editing the source code and building it  

Install the Lazarus IDE [from here](https://www.lazarus-ide.org/index.php?page=downloads) or from Linux repository. Windows is not supported yet.  
Clone the repository:  
`git clone https://github.com/artenax/ui`  
or download a zip  
In Lazarus choose File-Open  
At the bottom, choose the "Lazarus project *.lpi" filter  
ui.lpi  
Choose "Open as project"  
Change the blue lines in the source code editor to your liking  
Click Save all  
You can show logs in View-Messages  
Build your project: Run-Compile (Ctrl+F9)  
Compiled binary file ui will be in project folder (depends on GTK2)  

If after opening a project in Lazarus you don't see the source code (this might be the case if you chose "File - Close All" before), select:  
Project - Modules (Ctrl+F12) - main.pas  
Project - Forms (Shift+F12) - Form1  
You can move Form1, click Save All, compile, and your application will run in that position.

# Restrictions
Do not use double quotation marks in filenames and paths

![ui](https://user-images.githubusercontent.com/107228652/218859651-2688038b-47e0-48f6-ade2-5d81ec91d092.png)
