## Messaging systems, RPC & Web Services 

Messaging systems, RPC, and Web Services are three different ways of enabling communication between distributed components in a computer system. They have different advantages and disadvantages depending on the requirements and constraints of the system. Here is a brief comparison of them:

- **Messaging systems**: These are systems that allow asynchronous and reliable communication between components using messages that are stored and forwarded by intermediaries called brokers or message queues. Examples of messaging systems are Kafka, RabbitMQ, and ActiveMQ. Messaging systems have the following characteristics:
  - They decouple the sender and receiver of messages, allowing them to operate independently and at different speeds.
  - They provide fault tolerance and scalability by buffering messages in case of failures or congestion.
  - They support various message delivery patterns, such as point-to-point, publish-subscribe, or request-reply.
  - They require serialization and deserialization of messages, which can add overhead and complexity.
  - They may not guarantee the order or delivery of messages, depending on the configuration and quality of service.
- **RPC**: This stands for remote procedure call, which is a synchronous and request-response based communication method that allows a component to invoke a procedure or function on another component across the network. Examples of RPC frameworks are gRPC, Thrift, and RMI. RPC has the following characteristics:
  - It provides a simple and intuitive abstraction for distributed computing, as if calling a local procedure or function.
  - It supports various data formats and protocols, such as binary, JSON, XML, HTTP, or TCP.
  - It requires the sender and receiver to be available and reachable at the same time, which can introduce latency and dependency issues.
  - It may not handle failures or retries gracefully, unless explicitly implemented by the developer.
  - It may not be interoperable across different languages or platforms, unless using a common standard or specification.
- **Web Services**: These are services that expose their functionality through standardized web-based interfaces, such as REST or SOAP. Examples of web services are Google Maps API, Amazon S3 API, or Twitter API. Web services have the following characteristics:
  - They leverage the existing web infrastructure and protocols, such as HTTP, HTTPS, or SSL.
  - They are platform-independent and language-neutral, allowing for interoperability and integration across different systems.
  - They support various data formats and representations, such as XML, JSON, HTML, or plain text.
  - They may not provide high performance or efficiency, due to the overhead of web protocols and formats.
  - They may not support complex or stateful operations, due to the stateless and resource-oriented nature of web interfaces.

You can learn more about these communication methods and their trade-offs in ¹, ², ³, and .

Source: Conversation with Bing, 5/14/2023
(1) REST, RPC, and Brokered Messaging | by Nathan Murthy | Medium. https://medium.com/@natemurthy/rest-rpc-and-brokered-messaging-b775aeb0db3.
(2) rpc - What is the difference between remote procedure call and web .... https://stackoverflow.com/questions/3028899/what-is-the-difference-between-remote-procedure-call-and-web-service.
(3) REST vs RPC: What problems are you trying to solve with your APIs?. https://cloud.google.com/blog/products/application-development/rest-vs-rpc-what-problems-are-you-trying-to-solve-with-your-apis.

---


