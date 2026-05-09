import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Consumer;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.Envelope;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class Z1_Consumer {

    private static final String QUEUE_NAME = "queue1";

    private enum AckMode {
        AUTO,
        MANUAL
    }

    public static void main(String[] argv) throws Exception {

        // info
        System.out.println("Z1 CONSUMER");

        AckMode ackMode = AckMode.MANUAL;
        int qos = 0;
        String consumerName = "consumer-" + ProcessHandle.current().pid();
        for (String arg : argv) {
            if (arg.startsWith("--ack=")) {
                String value = arg.substring("--ack=".length()).toLowerCase();
                if ("auto".equals(value)) {
                    ackMode = AckMode.AUTO;
                } else if ("manual".equals(value)) {
                    ackMode = AckMode.MANUAL;
                } else {
                    throw new IllegalArgumentException("Unsupported ack mode: " + value + " (use auto/manual)");
                }
            } else if (arg.startsWith("--qos=")) {
                qos = Integer.parseInt(arg.substring("--qos=".length()));
                if (qos < 0) {
                    throw new IllegalArgumentException("QoS must be >= 0");
                }
            } else if (arg.startsWith("--name=")) {
                consumerName = arg.substring("--name=".length());
            } else {
                throw new IllegalArgumentException("Unknown argument: " + arg);
            }
        }

        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        Connection connection = factory.newConnection();
        Channel channel = connection.createChannel();

        channel.queueDeclare(QUEUE_NAME, false, false, false, null);
        if (qos > 0) {
            channel.basicQos(qos);
        }

        AckMode finalAckMode = ackMode;
        String finalConsumerName = consumerName;
        Consumer consumer = new DefaultConsumer(channel) {
            @Override
            public void handleDelivery(String consumerTag, Envelope envelope, AMQP.BasicProperties properties, byte[] body) throws IOException {
                String message = new String(body, StandardCharsets.UTF_8);
                long deliveryTag = envelope.getDeliveryTag();
                System.out.printf("[%s] Received: %s%n", finalConsumerName, message);

                int timeToSleep;
                try {
                    timeToSleep = Integer.parseInt(message.trim());
                } catch (NumberFormatException ex) {
                    System.out.printf("[%s] Invalid processing time (expected int seconds): %s%n", finalConsumerName, message);
                    if (finalAckMode == AckMode.MANUAL) {
                        channel.basicNack(deliveryTag, false, false);
                        System.out.printf("[%s] Rejected invalid message without requeue%n", finalConsumerName);
                    }
                    return;
                }

                System.out.printf("[%s] Processing for %d second(s)%n", finalConsumerName, timeToSleep);
                try {
                    Thread.sleep(timeToSleep * 1000L);
                } catch (InterruptedException ex) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Interrupted while processing message", ex);
                }

                System.out.printf("[%s] Processing finished%n", finalConsumerName);
                if (finalAckMode == AckMode.MANUAL) {
                    channel.basicAck(deliveryTag, false);
                    System.out.printf("[%s] Acknowledged message after processing%n", finalConsumerName);
                }
            }
        };

        boolean autoAck = ackMode == AckMode.AUTO;
        System.out.printf(
                "Waiting for messages (ack=%s, qos=%d, name=%s)...%n",
                ackMode.name().toLowerCase(),
                qos,
                consumerName
        );
        channel.basicConsume(QUEUE_NAME, autoAck, consumer);
    }
}
