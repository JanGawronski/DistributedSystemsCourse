import java.net.*;
import java.io.*;

public class UDPclient {
    public static void main(String[] args) {
	try (DatagramSocket socket = new DatagramSocket();) {
	    InetAddress address = InetAddress.getByName("localhost");
	    byte[] sendBuffer = "Ping".getBytes();
	    DatagramPacket sendPacket =
		new DatagramPacket(sendBuffer, sendBuffer.length, address, 9876);
	    socket.send(sendPacket);
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
}
