import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class Z1_Producer {

    private static final String QUEUE_NAME = "queue1";

    public static void main(String[] argv) throws Exception {

        System.out.println("Z1 PRODUCER");

        ConnectionFactory factory = new ConnectionFactory();
	
        Connection connection = factory.newConnection();
        Channel channel = connection.createChannel();

        channel.queueDeclare(QUEUE_NAME, false, false, false, null);

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        while (true) {
            System.out.print("Enter message (or 'exit'): ");
            String message = br.readLine();
            if (message == null || "exit".equalsIgnoreCase(message.trim())) {
                break;
            }

            channel.basicPublish("", QUEUE_NAME, null, message.getBytes(StandardCharsets.UTF_8));
            System.out.println("Sent: " + message);
        }

        channel.close();
        connection.close();
    }
}
