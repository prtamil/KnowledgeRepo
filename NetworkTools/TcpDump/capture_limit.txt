what is -c option mean ? how to use it ?
---------------------------------------

when writing to a file, we dont want to accidently fill 
the harddisk. so we limit the packets we capture

to capture 1000 packets

$ tcpdump -i eth0 -w capture.pcap -c 1000
