MODULE LinSockets ["libc.so.6"];
	(* BlackBox (Linux) Interface to sockets *)
	(* Copyright (c) Oberon microsystems, Inc. 2001 *)
	(* Header documentation from RedHat Linux distribution 7.1 *)

CONST
	(* From /usr/include/asm/errno.h *)
	EPERM* =  1; (* Operation not permitted *)
	ENOENT* =  2; (* No such file or directory *)
	ESRCH* =  3; (* No such process *)
	EINTR* =  4; (* Interrupted system call *)
	EIO* =  5; (* I/O error *)
	ENXIO* =  6; (* No such device or address *)
	E2BIG* =  7; (* Arg list too long *)
	ENOEXEC* =  8; (* Exec format error *)
	EBADF* =  9; (* Bad file number *)
	ECHILD* = 10; (* No child processes *)
	EAGAIN* = 11; (* Try again *)
	ENOMEM* = 12; (* Out of memory *)
	EACCES* = 13; (* Permission denied *)
	EFAULT* = 14; (* Bad address *)
	ENOTBLK* = 15; (* Block device required *)
	EBUSY* = 16; (* Device or resource busy *)
	EEXIST* = 17; (* File exists *)
	EXDEV* = 18; (* Cross-device link *)
	ENODEV* = 19; (* No such device *)
	ENOTDIR* = 20; (* Not a directory *)
	EISDIR* = 21; (* Is a directory *)
	EINVAL* = 22; (* Invalid argument *)
	ENFILE* = 23; (* File table overflow *)
	EMFILE* = 24; (* Too many open files *)
	ENOTTY* = 25; (* Not a typewriter *)
	ETXTBSY* = 26; (* Text file busy *)
	EFBIG* = 27; (* File too large *)
	ENOSPC* = 28; (* No space left on device *)
	ESPIPE* = 29; (* Illegal seek *)
	EROFS* = 30; (* Read-only file system *)
	EMLINK* = 31; (* Too many links *)
	EPIPE* = 32; (* Broken pipe *)
	EDOM* = 33; (* Math argument out of domain of func *)
	ERANGE* = 34; (* Math result not representable *)
	EDEADLK* = 35; (* Resource deadlock would occur *)
	ENAMETOOLONG* = 36; (* File name too long *)
	ENOLCK* = 37; (* No record locks available *)
	ENOSYS* = 38; (* Function not implemented *)
	ENOTEMPTY* = 39; (* Directory not empty *)
	ELOOP* = 40; (* Too many symbolic links encountered *)
	EWOULDBLOCK* = EAGAIN; (* Operation would block *)
	ENOMSG* = 42; (* No message of desired type *)
	EIDRM* = 43; (* Identifier removed *)
	ECHRNG* = 44; (* Channel number out of range *)
	EL2NSYNC* = 45; (* Level 2 not synchronized *)
	EL3HLT* = 46; (* Level 3 halted *)
	EL3RST* = 47; (* Level 3 reset *)
	ELNRNG* = 48; (* Link number out of range *)
	EUNATCH* = 49; (* Protocol driver not attached *)
	ENOCSI* = 50; (* No CSI structure available *)
	EL2HLT* = 51; (* Level 2 halted *)
	EBADE* = 52; (* Invalid exchange *)
	EBADR* = 53; (* Invalid request descriptor *)
	EXFULL* = 54; (* Exchange full *)
	ENOANO* = 55; (* No anode *)
	EBADRQC* = 56; (* Invalid request code *)
	EBADSLT* = 57; (* Invalid slot *)
	EDEADLOCK* = EDEADLK;
	EBFONT* = 59; (* Bad font file format *)
	ENOSTR* = 60; (* Device not a stream *)
	ENODATA* = 61; (* No data available *)
	ETIME* = 62; (* Timer expired *)
	ENOSR* = 63; (* Out of streams resources *)
	ENONET* = 64; (* Machine is not on the network *)
	ENOPKG* = 65; (* Package not installed *)
	EREMOTE* = 66; (* Object is remote *)
	ENOLINK* = 67; (* Link has been severed *)
	EADV* = 68; (* Advertise error *)
	ESRMNT* = 69; (* Srmount error *)
	ECOMM* = 70; (* Communication error on send *)
	EPROTO* = 71; (* Protocol error *)
	EMULTIHOP* = 72; (* Multihop attempted *)
	EDOTDOT* = 73; (* RFS specific error *)
	EBADMSG* = 74; (* Not a data message *)
	EOVERFLOW* = 75; (* Value too large for defined data type *)
	ENOTUNIQ* = 76; (* Name not unique on network *)
	EBADFD* = 77; (* File descriptor in bad state *)
	EREMCHG* = 78; (* Remote address changed *)
	ELIBACC* = 79; (* Can not access a needed shared library *)
	ELIBBAD* = 80; (* Accessing a corrupted shared library *)
	ELIBSCN* = 81; (* .lib section in a.out corrupted *)
	ELIBMAX* = 82; (* Attempting to link in too many shared libraries *)
	ELIBEXEC* = 83; (* Cannot exec a shared library directly *)
	EILSEQ* = 84; (* Illegal byte sequence *)
	ERESTART* = 85; (* Interrupted system call should be restarted *)
	ESTRPIPE* = 86; (* Streams pipe error *)
	EUSERS* = 87; (* Too many users *)
	ENOTSOCK* = 88; (* Socket operation on non-socket *)
	EDESTADDRREQ* = 89; (* Destination address required *)
	EMSGSIZE* = 90; (* Message too long *)
	EPROTOTYPE* = 91; (* Protocol wrong type for socket *)
	ENOPROTOOPT* = 92; (* Protocol not available *)
	EPROTONOSUPPORT* = 93; (* Protocol not supported *)
	ESOCKTNOSUPPORT* = 94; (* Socket type not supported *)
	EOPNOTSUPP* = 95; (* Operation not supported on transport endpoint *)
	EPFNOSUPPORT* = 96; (* Protocol family not supported *)
	EAFNOSUPPORT* = 97; (* Address family not supported by protocol *)
	EADDRINUSE* = 98; (* Address already in use *)
	EADDRNOTAVAIL* = 99; (* Cannot assign requested address *)
	ENETDOWN* = 100; (* Network is down *)
	ENETUNREACH* = 101; (* Network is unreachable *)
	ENETRESET* = 102; (* Network dropped connection because of reset *)
	ECONNABORTED* = 103; (* Software caused connection abort *)
	ECONNRESET* = 104; (* Connection reset by peer *)
	ENOBUFS* = 105; (* No buffer space available *)
	EISCONN* = 106; (* Transport endpoint is already connected *)
	ENOTCONN* = 107; (* Transport endpoint is not connected *)
	ESHUTDOWN* = 108; (* Cannot send after transport endpoint shutdown *)
	ETOOMANYREFS* = 109; (* Too many references: cannot splice *)
	ETIMEDOUT* = 110; (* Connection timed out *)
	ECONNREFUSED* = 111; (* Connection refused *)
	EHOSTDOWN* = 112; (* Host is down *)
	EHOSTUNREACH* = 113; (* No route to host *)
	EALREADY* = 114; (* Operation already in progress *)
	EINPROGRESS* = 115; (* Operation now in progress *)
	ESTALE* = 116; (* Stale NFS file handle *)
	EUCLEAN* = 117; (* Structure needs cleaning *)
	ENOTNAM* = 118; (* Not a XENIX named type file *)
	ENAVAIL* = 119; (* No XENIX semaphores available *)
	EISNAM* = 120; (* Is a named type file *)
	EREMOTEIO* = 121; (* Remote I/O error *)
	EDQUOT* = 122; (* Quota exceeded *)
	ENOMEDIUM* = 123; (* No medium found *)
	EMEDIUMTYPE* = 124; (* Wrong medium type *)

	(* Linsock specific *)
	INVALID_SOCKET* = 0FFFFFFFFH;
	SOCKET_ERROR* = -1;

	(* From /usr/include/bits/socket.h *)
	SOCK_STREAM* = 1; (* Sequenced, reliable, connection-based byte streams. *)
	SOCK_DGRAM* = 2; (* Connectionless, unreliable datagrams of fixed maximum length. *)
	SOCK_RAW* = 3; (* Raw protocol interface. *)
	SOCK_RDM* = 4; (* Reliably-delivered messages. *)
	SOCK_SEQPACKET* = 5; (* Sequenced, reliable, connection-based, datagrams of fixed maximum length. *)
	SOCK_PACKET* = 10; (* Linux specific way of getting packets at the dev level. *)

	(* Protocol families. *)
	PF_UNSPEC* = 0; (* Unspecified. *)
	PF_LOCAL* = 1; (* Local to host (pipes and file-domain). *)
	PF_UNIX* = PF_LOCAL; (* Old BSD name for PF_LOCAL. *)
	PF_FILE* = PF_LOCAL; (* Another non-standard name for PF_LOCAL. *)
	PF_INET* = 2; (* IP protocol family. *)
	PF_AX25* = 3; (* Amateur Radio AX.25. *)
	PF_IPX* = 4; (* Novell Internet Protocol. *)
	PF_APPLETALK* = 5; (* Appletalk DDP. *)
	PF_NETROM* = 6; (* Amateur radio NetROM. *)
	PF_BRIDGE* = 7; (* Multiprotocol bridge. *)
	PF_ATMPVC* = 8; (* ATM PVCs. *)
	PF_X25* = 9; (* Reserved for X.25 project. *)
	PF_INET6* = 10; (* IP version 6. *)
	PF_ROSE* = 11; (* Amateur Radio X.25 PLP. *)
	PF_DECnet* = 12; (* Reserved for DECnet project. *)
	PF_NETBEUI* = 13; (* Reserved for 802.2LLC project. *)
	PF_SECURITY* = 14; (* Security callback pseudo AF. *)
	PF_KEY* = 15; (* PF_KEY key management API. *)
	PF_NETLINK* = 16;
	PF_ROUTE* = PF_NETLINK; (* Alias to emulate 4.4BSD. *)
	PF_PACKET* = 17; (* Packet family. *)
	PF_ASH* = 18; (* Ash. *)
	PF_ECONET* = 19; (* Acorn Econet. *)
	PF_ATMSVC* = 20; (* ATM SVCs. *)
	PF_SNA* = 22; (* Linux SNA Project *)
	PF_IRDA* = 23; (* IRDA sockets. *)
	PF_PPPOX* = 24; (* PPPoX sockets. *)
	PF_MAX* = 32; (* For now.. *)

	(* Address families. *)
	AF_UNSPEC* = PF_UNSPEC;
	AF_LOCAL* = PF_LOCAL;
	AF_UNIX* = PF_UNIX;
	AF_FILE* = PF_FILE;
	AF_INET* = PF_INET;
	AF_AX25* = PF_AX25;
	AF_IPX* = PF_IPX;
	AF_APPLETALK* = PF_APPLETALK;
	AF_NETROM* = PF_NETROM;
	AF_BRIDGE* = PF_BRIDGE;
	AF_ATMPVC* = PF_ATMPVC;
	AF_X25* = PF_X25;
	AF_INET6* = PF_INET6;
	AF_ROSE* = PF_ROSE;
	AF_DECnet* = PF_DECnet;
	AF_NETBEUI* = PF_NETBEUI;
	AF_SECURITY* = PF_SECURITY;
	AF_KEY* = PF_KEY;
	AF_NETLINK* = PF_NETLINK;
	AF_ROUTE* = PF_ROUTE;
	AF_PACKET* = PF_PACKET;
	AF_ASH* = PF_ASH;
	AF_ECONET* = PF_ECONET;
	AF_ATMSVC* = PF_ATMSVC;
	AF_SNA* = PF_SNA;
	AF_IRDA* = PF_IRDA;
	AF_PPPOX* = PF_PPPOX;
	AF_MAX* = PF_MAX;

	(* Socket level values. *)
	SOL_RAW* = 255;
	SOL_DECNET* = 261;
	SOL_X25* = 262;
	SOL_PACKET* = 263;
	SOL_ATM* = 264; (* ATM layer (cell level). *)
	SOL_AAL* = 265; (* ATM Adaption Layer (packet level). *)
	SOL_IRDA* = 266;

	SOMAXCONN* =128; (* Maximum queue length specifiable by listen. *)

	(* Bits in the FLAGS argument to `send', `recv', et al. *)
	MSG_OOB* = 0001H; (* Process out-of-band data. *)
	MSG_PEEK* = 0002H; (* Peek at incoming messages. *)
	MSG_DONTROUTE* = 0004H; (* Don't use local routing. *)
	MSG_TRYHARD* = MSG_DONTROUTE; (* DECnet uses a different name. *)
	MSG_CTRUNC* = 0008H; (* Control data lost before delivery. *)
	MSG_PROXY* = 0010H; (* Supply or ask second address. *)
	MSG_TRUNC* = 0020H;
	MSG_DONTWAIT* = 0040H; (* Nonblocking IO. *)
	MSG_EOR* = 0080H; (* End of record. *)
	MSG_WAITALL* = 0100H; (* Wait for a full request. *)
	MSG_FIN* = 0200H;
	MSG_SYN* = 0400H;
	MSG_CONFIRM* = 0800H; (* Confirm path validity. *)
	MSG_RST* = 1000H;
	MSG_ERRQUEUE* = 2000H; (* Fetch message from error queue. *)
	MSG_NOSIGNAL* = 4000H; (* Do not generate SIGPIPE. *)

	(* Socket level message types *)
	SCM_RIGHTS* = 0001H;
	SCM_CREDENTIALS* = 0002H;
	__SCM_CONNECT* = 0003H;

	(* From /usr/include/asm/sockios.h *)
	(* Socket-level I/O control calls. *)
	FIOSETOWN* = 8901H;
	SIOCSPGRP* = 8902H;
	FIOGETOWN* = 8903H;
	SIOCGPGRP* = 8904H;
	SIOCATMARK* = 8905H;
	SIOCGSTAMP* = 8906H;

	(* From /usr/include/asm/socket.h *)
	SOL_SOCKET* = 1;

	SO_DEBUG* = 1;
	SO_REUSEADDR* = 2;
	SO_TYPE* = 3;
	SO_ERROR* = 4;
	SO_DONTROUTE* = 5;
	SO_BROADCAST* = 6;
	SO_SNDBUF* = 7;
	SO_RCVBUF* = 8;
	SO_KEEPALIVE* = 9;
	SO_OOBINLINE* = 10;
	SO_NO_CHECK* = 11;
	SO_PRIORITY* = 12;
	SO_LINGER* = 13;
	SO_BSDCOMPAT* = 14;
	SO_REUSEPORT* = 15;
	SO_PASSCRED* = 16;
	SO_PEERCRED* = 17;
	SO_RCVLOWAT* = 18;
	SO_SNDLOWAT* = 19;
	SO_RCVTIMEO* = 20;
	SO_SNDTIMEO* = 21;
	SO_SECURITY_AUTHENTICATION* = 22;
	SO_SECURITY_ENCRYPTION_TRANSPORT* = 23;
	SO_SECURITY_ENCRYPTION_NETWORK* = 24;
	SO_BINDTODEVICE* = 25;
	SO_ATTACH_FILTER* = 26;
	SO_DETACH_FILTER* = 27;
	SO_PEERNAME* = 28;
	SO_TIMESTAMP* = 29;
	SCM_TIMESTAMP* = SO_TIMESTAMP;
	SO_ACCEPTCONN* = 30;

	(* From /usr/include/netinet/in.h *)
	(* Standard well-defined IP protocols. *)
	IPPROTO_IP* = 0; (* Dummy protocol for TCP. *)
	IPPROTO_HOPOPTS* = 0; (* IPv6 Hop-by-Hop options. *)
	IPPROTO_ICMP* = 1; (* Internet Control Message Protocol. *)
	IPPROTO_IGMP* = 2; (* Internet Group Management Protocol. *)
	IPPROTO_IPIP* = 4; (* IPIP tunnels (older KA9Q tunnels use 94). *)
	IPPROTO_TCP* = 6; (* Transmission Control Protocol. *)
	IPPROTO_EGP* = 8; (* Exterior Gateway Protocol. *)
	IPPROTO_PUP* = 12; (* PUP protocol. *)
	IPPROTO_UDP* = 17; (* User Datagram Protocol. *)
	IPPROTO_IDP* = 22; (* XNS IDP protocol. *)
	IPPROTO_TP* = 29; (* SO Transport Protocol Class 4. *)
	IPPROTO_IPV6* = 41; (* IPv6 header. *)
	IPPROTO_ROUTING* = 43; (* IPv6 routing header. *)
	IPPROTO_FRAGMENT* = 44; (* IPv6 fragmentation header. *)
	IPPROTO_RSVP* = 46; (* Reservation Protocol. *)
	IPPROTO_GRE* = 47; (* General Routing Encapsulation. *)
	IPPROTO_ESP* = 50; (* encapsulating security payload. *)
	IPPROTO_AH* = 51; (* authentication header. *)
	IPPROTO_ICMPV6* = 58; (* ICMPv6. *)
	IPPROTO_NONE* = 59; (* IPv6 no next header. *)
	IPPROTO_DSTOPTS* = 60; (* IPv6 destination options. *)
	IPPROTO_MTP* = 92; (* Multicast Transport Protocol. *)
	IPPROTO_ENCAP* = 98; (* Encapsulation Header. *)
	IPPROTO_PIM* = 103; (* Protocol Independent Multicast. *)
	IPPROTO_COMP* = 108; (* Compression Header Protocol. *)
	IPPROTO_RAW* = 255; (* Raw IP packets. *)
	IPPROTO_MAX* = IPPROTO_RAW + 1; 

	(* Standard well-known ports. *)
	IPPORT_ECHO* = 7; (* Echo service.*)
	IPPORT_DISCARD* = 9; (* Discard transmissions service.*)
	IPPORT_SYSTAT* = 11; (* System status service.*)
	IPPORT_DAYTIME* = 13; (* Time of day service.*)
	IPPORT_NETSTAT* = 15; (* Network status service.*)
	IPPORT_FTP* = 21; (* File Transfer Protocol.*)
	IPPORT_TELNET* = 23; (* Telnet protocol.*)
	IPPORT_SMTP* = 25; (* Simple Mail Transfer Protocol.*)
	IPPORT_TIMESERVER* = 37; (* Timeserver service.*)
	IPPORT_NAMESERVER* = 42; (* Domain Name Service.*)
	IPPORT_WHOIS* = 43; (* Internet Whois service.*)
	IPPORT_MTP* = 57; 
	IPPORT_TFTP* = 69; (* Trivial File Transfer Protocol.*)
	IPPORT_RJE* = 77; 
	IPPORT_FINGER* = 79; (* Finger service.*)
	IPPORT_TTYLINK* = 87; 
	IPPORT_SUPDUP* = 95; (* SUPDUP protocol.*)
	IPPORT_EXECSERVER* = 512; (* execd service.*)
	IPPORT_LOGINSERVER* = 513; (* rlogind service.*)
	IPPORT_CMDSERVER* = 514; 
	IPPORT_EFSSERVER* = 520; 
	
	(* UDP ports.*)
	IPPORT_BIFFUDP* = 512; 
	IPPORT_WHOSERVER* = 513; 
	IPPORT_ROUTESERVER* = 520; 
	
	(* Ports less than this value are reserved for privileged processes.*)
	IPPORT_RESERVED* = 1024; 
	
	(* Ports greater this value are reserved for (non-privileged) servers.*)
	IPPORT_USERRESERVED* = 5000;

	(* From /usr/include/netinet/in.h *)
	(* Definitions of the bits in an Internet address integer. *)
	IN_CLASSA_NET* = 0FF000000H;
	IN_CLASSA_NSHIFT* = 24;
	IN_CLASSA_HOST* = 0FFFFFFH;
	IN_CLASSA_MAX* = 128;
	IN_CLASSB_NET* = 0FFFF0000H;
	IN_CLASSB_NSHIFT* = 16;
	IN_CLASSB_HOST* = 0FFFFH;
	IN_CLASSB_MAX* = 65536;
	IN_CLASSC_NET* = 0FFFFFF00H;
	IN_CLASSC_NSHIFT* = 8;
	IN_CLASSC_HOST* = 0FFH;
	INADDR_ANY* = 0H; (* Address to accept any incoming messages. *)
	INADDR_BROADCAST* = 0FFFFFFFFH; (* Address to send to all hosts. *)
	INADDR_NONE* = 0FFFFFFFFH; (* Address indicating an error return. *)
	IN_LOOPBACKNET* = 7FH; (* Network number for local host loopback. *)
	INADDR_LOOPBACK* = 7F000001H; (* Address to loopback in software to local host. *)

	(* Definitions for Multicast INADDR. *)
	INADDR_UNSPEC_GROUP* = 0E0000000H; (* 224.0.0.0 *)
	INADDR_ALLHOSTS_GROUP* = 0E0000001H; (* 224.0.0.1 *)
	INADDR_ALLRTRS_GROUP* = 0E0000002H; (* 224.0.0.2 *)
	INADDR_MAX_LOCAL_GROUP* = 0E00000FFH; (* 224.0.0.255 *)

	(* From /usr/include/bits/ioctls.h *)
	SIOCADDRT* = 890BH; (* add routing table entry *)
	SIOCDELRT* = 890CH; (* delete routing table entry *)
	SIOCRTMSG* = 890DH; (* call to routing system *)
	SIOCGIFNAME* = 8910H; (* get iface name *)
	SIOCSIFLINK* = 8911H; (* set iface channel *)
	SIOCGIFCONF* = 8912H; (* get iface list *)
	SIOCGIFFLAGS* = 8913H; (* get flags *)
	SIOCSIFFLAGS* = 8914H; (* set flags *)
	SIOCGIFADDR* = 8915H; (* get PA address *)
	SIOCSIFADDR* = 8916H; (* set PA address *)
	SIOCGIFDSTADDR* = 8917H; (* get remote PA address *)
	SIOCSIFDSTADDR* = 8918H; (* set remote PA address *)
	SIOCGIFBRDADDR* = 8919H; (* get broadcast PA address *)
	SIOCSIFBRDADDR* = 891AH; (* set broadcast PA address *)
	SIOCGIFNETMASK* = 891BH; (* get network PA mask *)
	SIOCSIFNETMASK* = 891CH; (* set network PA mask *)
	SIOCGIFMETRIC* = 891DH; (* get metric *)
	SIOCSIFMETRIC* = 891EH; (* set metric *)
	SIOCGIFMEM* = 891FH; (* get memory address (BSD) *)
	SIOCSIFMEM* = 8920H; (* set memory address (BSD) *)
	SIOCGIFMTU* = 8921H; (* get MTU size *)
	SIOCSIFMTU* = 8922H; (* set MTU size *)
	SIOCSIFHWADDR* = 8924H; (* set hardware address *)
	SIOCGIFENCAP* = 8925H; (* get/set encapsulations      *)
	SIOCSIFENCAP* = 8926;
	SIOCGIFHWADDR* = 8927H; (* Get hardware address *)
	SIOCGIFSLAVE* = 8929H; (* Driver slaving support *)
	SIOCSIFSLAVE* = 8930;
	SIOCADDMULTI* = 8931H; (* Multicast address lists *)
	SIOCDELMULTI* = 8932;
	SIOCGIFINDEX* = 8933H; (* name -> if_index mapping *)
	SIOCSIFPFLAGS* = 8934H; (* set/get extended flags set *)
	SIOCGIFPFLAGS* = 8935;
	SIOCDIFADDR* = 8936H; (* delete PA address *)
	SIOCSIFHWBROADCAST* = 8937H; (* set hardware broadcast addr *)
	SIOCGIFCOUNT* = 8938H; (* get number of devices *)
	SIOCGIFBR* = 8940H; (* Bridging support *)
	SIOCSIFBR* = 8941H; (* Set bridging options *)
	SIOCGIFTXQLEN* = 8942H; (* Get the tx queue length *)
	SIOCSIFTXQLEN* = 8943H; (* Set the tx queue length *)
	SIOCDARP* = 8953H; (* delete ARP table entry *)
	SIOCGARP* = 8954H; (* get ARP table entry *)
	SIOCSARP* = 8955H; (* set ARP table entry *)
	SIOCDRARP* = 8960H; (* delete RARP table entry *)
	SIOCGRARP* = 8961H; (* get RARP table entry *)
	SIOCSRARP* = 8962H; (* set RARP table entry *)
	SIOCGIFMAP* = 8970H; (* Get device parameters *)
	SIOCSIFMAP* = 8971H; (* Set device parameters *)
	SIOCADDDLCI* = 8980H; (* Create new DLCI device *)
	SIOCDELDLCI* = 8981H; (* Delete DLCI device *)
	SIOCDEVPRIVATE* = 89F0H; (* to 89FF *)
	SIOCPROTOPRIVATE* = 89E0H; (* to 89EF *)
	
	(* From /usr/include/asm/ioctls.h *)
	TIOCEXCL* = 540CH;
	TIOCNXCL* = 540DH;
	TIOCSCTTY* = 540EH;
	TIOCGPGRP* = 540FH;
	TIOCSPGRP* = 5410H;
	TIOCOUTQ* = 5411H;
	TIOCSTI* = 5412H;
	TIOCGWINSZ* = 5413H;
	TIOCSWINSZ* = 5414H;
	TIOCMGET* = 5415H;
	TIOCMBIS* = 5416H;
	TIOCMBIC* = 5417H;
	TIOCMSET* = 5418H;
	TIOCGSOFTCAR* = 5419H;
	TIOCSSOFTCAR* = 541AH;
	FIONREAD* = 541BH;
	TIOCINQ* = FIONREAD;
	TIOCLINUX* = 541CH;
	TIOCCONS* = 541DH;
	TIOCGSERIAL* = 541EH;
	TIOCSSERIAL* = 541FH;
	TIOCPKT* = 5420H;
	FIONBIO* = 5421H;
	TIOCNOTTY* = 5422H;
	TIOCSETD* = 5423H;
	TIOCGETD* = 5424H;
	TIOCTTYGSTRUCT* = 5426H; (* For debugging only *)
	TIOCSBRK* = 5427H; (* BSD compatibility *)
	TIOCCBRK* = 5428H; (* BSD compatibility *)
	TIOCGSID* = 5429H; (* Return the session ID of FD *)
	TIOCGPTN* = 80045430H; (* Get Pty Number (of pty-mux device) *)
	TIOCSPTLCK* = 40045431H; (* Lock/unlock Pty *)
	FIONCLEX* = 5450H; (* these numbers need to be adjusted. *)
	FIOCLEX* = 5451H;
	FIOASYNC* = 5452H;
	TIOCSERCONFIG* = 5453H;
	TIOCSERGWILD* = 5454H;
	TIOCSERSWILD* = 5455H;
	TIOCGLCKTRMIOS* = 5456H;
	TIOCSLCKTRMIOS* = 5457H;
	TIOCSERGSTRUCT* = 5458H; (* For debugging only *)
	TIOCSERGETLSR* = 5459H; (* Get line status register *)
	TIOCSERGETMULTI* = 545AH; (* Get multiport config *)
	TIOCSERSETMULTI* = 545BH; (* Set multiport config *)
	TIOCMIWAIT* = 545CH; (* wait for a change on serial input line(s) *)
	TIOCGICOUNT* = 545DH; (* read serial port inline interrupt counts *)
	TIOCGHAYESESP* = 545EH; (* Get Hayes ESP configuration *)
	TIOCSHAYESESP* = 545FH; (* Set Hayes ESP configuration *)
	FIOQSIZE* = 5460H;
	TIOCPKT_DATA* = 0;
	TIOCPKT_FLUSHREAD* = 1;
	TIOCPKT_FLUSHWRITE* = 2;
	TIOCPKT_STOP* = 4;
	TIOCPKT_START* = 8;
	TIOCPKT_NOSTOP* = 16;
	TIOCPKT_DOSTOP* = 32;
	TIOCSER_TEMT* = 0001H; (* Transmitter physically empty *)
	
	(* From /usr/include/asm/ioctl.h *)
	IOC_IN* = 40000000H;
	IOC_OUT* = 80000000H;
	IOC_INOUT* = 0C0000000H;
	IOCSIZE_MASK* = 3FFF0000H;
	IOCSIZE_SHIFT* = 0010H;
	
	(* From /usr/include/bits/ioctl-types.h *)
	TIOCM_LE* = 0001H;
	TIOCM_DTR* = 0002H;
	TIOCM_RTS* = 0004H;
	TIOCM_ST* = 0008H;
	TIOCM_SR* = 0010H;
	TIOCM_CTS* = 0020H;
	TIOCM_CAR* = 0040H;
	TIOCM_RNG* = 0080H;
	TIOCM_DSR* = 0100H;
	TIOCM_CD* = TIOCM_CAR;
	TIOCM_RI* = TIOCM_RNG;

	(* From /usr/include/sys/socket.h *)
	(* For the second parameter of shutdown. *)
	SHUT_RD* = 0;
	SHUT_WR* = 1;
	SHUT_RDWR* = 2;
	
	(* From /usr/include/bits/socket.h *)
	_SS_SIZE* = 128;
	_SS_PADSIZE* = _SS_SIZE - (2 * 4); (* Assumes __uint32_t. __ss_aligntype is 4 bytes. *)

	(* From /usr/indclude/sys/select.h *)
	FD_SETSIZE* = 1024;

TYPE
	ADDRESS* = INTEGER; (* For parameter __buf *)
	SOCKET* = INTEGER;
	StringPtr* = POINTER TO ARRAY [untagged] OF SHORTCHAR;
	uint16_t* = SHORTINT;
	uint32_t* = INTEGER;
	size_t* = INTEGER;
	__socklen_t* = INTEGER; (* Type for length arguments in socket calls. *)
	socklen_t* = __socklen_t;
	sa_family_t* = SHORTINT;
	__ss_aligntype* = INTEGER; (* Assumes __uint32_t, as opposed to __uint64_t *)

	sockaddr_storage* = RECORD [untagged]
		ss_family*: sa_family_t;
		__ss_align*: __ss_aligntype;
		__ss_padding*: ARRAY _SS_PADSIZE OF SHORTCHAR;
	END;

	iovec* = RECORD [untagged]
		iov_base*: StringPtr;
		iov_len*: size_t;
	END;

	pid_t* = INTEGER;
	uid_t* = INTEGER;
	gid_t* = INTEGER;
	ucred* = RECORD;
		pid*: pid_t;
		uid*: uid_t;
		gid*: gid_t
	END;

	linger* = RECORD [untagged]
		l_onoff*: INTEGER;
		l_linger*: INTEGER;
	END;

	in_addr_t* = INTEGER;
	in_addr* = RECORD [untagged]
		s_addr*: in_addr_t;
	END;

	sockaddr_in* = RECORD [untagged]
		sin_family*: SHORTINT;
		sin_port*: SHORTINT;
		sin_addr*: in_addr;
		sin_zero*: ARRAY [untagged] 8 OF SHORTCHAR;
	END;

	__time_t*= INTEGER;
	__suseconds_t*= INTEGER;
	timeval* = RECORD [untagged]
		tv_sec*: __time_t;
		tv_usec*: __suseconds_t;
	END;

	__fd_mask* = SET;
	fd_set* = RECORD [untagged]
		__fds_bits*: ARRAY [untagged] 32 OF __fd_mask
	END;

	hostentPtr* = POINTER TO hostent;	
	hostent* = RECORD [untagged]
		h_name*: StringPtr;
		h_aliases*: POINTER TO ARRAY [untagged] OF StringPtr;
		h_addrtype*: INTEGER;
		h_length*: INTEGER;
		h_addr_list*: POINTER TO ARRAY [untagged] OF in_addr;
	END;

PROCEDURE ntohl* (__netlong: uint32_t): INTEGER;
(*END ntohl;*)

PROCEDURE ntohs* (__netshort: uint16_t): SHORTINT;
(*END ntohs;*)

PROCEDURE htonl* (__hostlong: uint32_t): INTEGER;
(*END htonl;*)

PROCEDURE htons* (__hostshort: uint16_t): SHORTINT;
(*END htons;*)

PROCEDURE socket* (__domain, __type, __protocol: INTEGER): SOCKET;
(*END socket;*)

PROCEDURE bind* (__fd: SOCKET; VAR __addr: sockaddr_in; __len: socklen_t): INTEGER;
(*END bind;*)

PROCEDURE getsockname* (__fd: SOCKET; VAR __addr: sockaddr_in; VAR __len: socklen_t): INTEGER;
(*END getsockname;*)

PROCEDURE connect* (__fd: SOCKET; VAR __addr: sockaddr_in; __len: socklen_t): INTEGER;
(*END connect;*)

PROCEDURE getpeername* (__fd: SOCKET; VAR __addr: sockaddr_in; VAR __len: socklen_t): INTEGER;
(*END getpeername;*)

PROCEDURE send* (__fd: SOCKET; __buf: ADDRESS; __n: size_t; __flags: SET): INTEGER;
(*END send;*)

PROCEDURE recv* (__fd: SOCKET; __buf: ADDRESS; __n: size_t; __flags: SET): INTEGER;
(*END recv;*)

PROCEDURE sendto* (__fd: SOCKET; __buf: ADDRESS; __n: size_t;
	__flags: SET; VAR [nil] __addr: sockaddr_in; __addr_len: socklen_t): INTEGER;
(*END sendto;*)

PROCEDURE recvfrom* (__fd: SOCKET; __buf: ADDRESS; __n: size_t; __flags: SET;
					VAR [nil] __addr: sockaddr_in; VAR [nil] __addr_len: socklen_t): INTEGER;
(*END recvfrom;*)

PROCEDURE getsockopt* (__fd: SOCKET; __level, __optname: INTEGER; __optval: ADDRESS; VAR __optlen: socklen_t): INTEGER;
(*END getsockopt;*)

PROCEDURE setsockopt* (__fd: SOCKET; __level, __optname: INTEGER; __optval: ADDRESS; __optlen: socklen_t): INTEGER;
(*END setsockopt;*)

PROCEDURE listen* (__fd: SOCKET; __n: INTEGER): INTEGER;
(*END listen;*)

PROCEDURE accept* (__fd: SOCKET; VAR [nil] __addr: sockaddr_in; VAR [nil] __addr_len: socklen_t): SOCKET;
(*END accept;*)

PROCEDURE shutdown* (__fd: SOCKET; __how: INTEGER): INTEGER;
(*END shutdown;*)

PROCEDURE inet_addr* (__cp: StringPtr): in_addr_t; 
(*END inet_addr;*)

PROCEDURE inet_ntoa* (__in: in_addr): StringPtr;
(*END inet_ntoa;*)

PROCEDURE inet_aton* (__cp: StringPtr; VAR __inp: in_addr): in_addr_t;
(*END inet_aton;*)

PROCEDURE select* (__nfds: INTEGER; VAR [nil] __readfds, __writefds, __exceptfds: fd_set; VAR [nil] __timeout: timeval): INTEGER;
(*END select;*)

PROCEDURE gethostbyaddr* (VAR __addr: in_addr; __len: __socklen_t; __type: INTEGER): hostentPtr;
(*END gethostbyaddr;*)

PROCEDURE gethostbyname* (__name: StringPtr): hostentPtr;
(*END gethostbyname;*)

PROCEDURE gethostname* (__name: StringPtr; __len: size_t): INTEGER;
(*END gethostname;*)

PROCEDURE close* (__fd: SOCKET): INTEGER;
(*END close;*)

PROCEDURE ioctl* (__fd: SOCKET; __request: INTEGER; VAR argp: INTEGER): INTEGER;
(*END ioctl;*)

(* Linsock specific calls *)

PROCEDURE LinStartup* (versionReq: INTEGER; VAR LinData: INTEGER): INTEGER;
(*END LinStartup;*)

PROCEDURE LinCleanup* (): INTEGER;
(*END LinCleanup;*)

PROCEDURE LinGetLastError* (): INTEGER;
(*END LinGetLastError;*)

END LinSockets.
