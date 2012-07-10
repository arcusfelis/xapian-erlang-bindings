

#Module xapian_port#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)





<a name="types"></a>

##Data Types##




###<a name="type-x_port">x_port()</a>##



<pre>x_port() = #port_rec{}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close a port.</td></tr><tr><td valign="top"><a href="#connect-2">connect/2</a></td><td>Change the process's owner.</td></tr><tr><td valign="top"><a href="#control-3">control/3</a></td><td>Send a command.</td></tr><tr><td valign="top"><a href="#is_port_alive-1">is_port_alive/1</a></td><td>Return true, if the passed port is active.</td></tr><tr><td valign="top"><a href="#open-1">open/1</a></td><td>Open an instance (port).</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="close-1"></a>

###close/1##




`close(Port_rec) -> any()`





Close a port.  
If Port = port:  
* Release resources;  
* Close the port program;

If port = driver:
* Release resources.<a name="connect-2"></a>

###connect/2##




`connect(Port_rec, NewOwnerPid) -> any()`



Change the process's owner.<a name="control-3"></a>

###control/3##




`control(Port_rec, Command, Data) -> any()`



Send a command.<a name="is_port_alive-1"></a>

###is_port_alive/1##




`is_port_alive(Port_rec) -> any()`



Return true, if the passed port is active.<a name="open-1"></a>

###open/1##




`open(Type) -> any()`



Open an instance (port).