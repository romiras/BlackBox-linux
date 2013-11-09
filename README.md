BlackBox-linux
==============

A Linux port of BlackBox Component Builder

Initial development of Linux port started in 2001 with reference to BlackBox Component Builder 1.4 for Windows.


> The Linux version has been developed on RedHat Linux 7.1, this release uses version 2.4.2-2 of the Linux kernel and Libc. It also comes with Gnome 1.0 installed, which in turn uses version 1.2 of the Gtk, Gdk and GLib libraries.
> 
> BlackBox has not been tested on other Linux distributions than RedHat, but it should only depend on the Libc, Gnome, Gtk, Gdk and GLib versions. Any distribution where Gnome is available should work with the BlackBox port.

Subsystem Host is incomplete and requires further development.

The current state of the project:

* Run-time system (module Kernel) is ported;
* Non-GUI modules are ported;
* GUI framework will not run on modern Linux distributions since it is work only on Gtk 1.x. Not runnable on Gnome 2.x and later versions.
* Some parts are X-Windows specific.

Porting Experiences
----------------------

> All in all it was surprisingly easy to port BlackBox to Linux. After just porting some of the most basic modules most frameworks within BlackBox already start working. Only the modules Kernel, HostFiles, HostPorts and HostWindows are needed to display a view. By adding HostMenus already ObxCube runs. And this without any of the mentioned modules being complete.

Related projects
------------------

* https://github.com/aixp/Blackbox/ - Port for OpenBSD/i386, GNU/Linux/i386, FreeBSD/i386 (console only)
