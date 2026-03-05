import socket
serverSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
serverSocket.bind(('', 9009))
while True:
    buff, address = serverSocket.recvfrom(1024)
    print("received msg: " + str(buff, 'utf-8'))
