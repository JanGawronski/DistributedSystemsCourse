import socket
client = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
client.sendto(bytes("Ping Python Udp!", 'utf-8'), ("127.0.0.1", 9009))
