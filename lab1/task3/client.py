import socket
client = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
client.sendto((300).to_bytes(4, byteorder='little'), ("127.0.0.1", 9876))
buff = client.recv(1024)
print(int.from_bytes(buff, byteorder='little'))
