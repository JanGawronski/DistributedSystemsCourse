import java.net.*;
import java.io.*;

public class TCPclient {
    public static void main(String[] args) {
	try (Socket socket = new Socket("localhost", 12345)) {
	    PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
	    BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
	    out.println("Ping");
	    String response = in.readLine();
	    System.out.println("received msg: " + response);
	} catch (IOException e) {
	    e.printStackTrace();
	}
    }
}
