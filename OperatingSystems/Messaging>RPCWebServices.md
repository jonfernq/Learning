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
> Is RPC used for much nowadays ? 

RPC is still used nowadays for various purposes, especially in distributed systems that require high performance, efficiency, and low latency. Some examples of RPC usage are:

- gRPC: This is a modern, open-source RPC framework developed by Google that uses HTTP/2 and protocol buffers to enable fast and scalable communication between microservices. It supports various languages and platforms, such as C++, Java, Python, Go, Node.js, Android, and iOS. It also provides features such as authentication, load balancing, logging, and monitoring¹.
- RMI: This stands for remote method invocation, which is a Java-specific implementation of RPC that allows objects to invoke methods on remote objects across the network. It uses object serialization and stubs to facilitate the communication between Java virtual machines. It also supports features such as garbage collection, security, and dynamic class loading².
- DCOM: This stands for distributed component object model, which is a Microsoft technology that extends the component object model (COM) to support communication between COM components across the network. It uses a binary protocol and stubs to enable RPC between Windows applications. It also supports features such as security, transactions, events, and activation³.

You can learn more about these and other RPC frameworks and technologies in ¹, ², ³, and .

Source: Conversation with Bing, 5/14/2023
(1) Remote procedure call (RPC) - Win32 apps | Microsoft Learn. https://learn.microsoft.com/en-us/windows/win32/rpc/rpc-start-page.
(2) Remote procedure call - Wikipedia. https://en.wikipedia.org/wiki/Remote_procedure_call.
(3) Remote Procedure Call (RPC) Protocol in Distributed System - Guru99. https://www.guru99.com/remote-procedure-call-rpc.html.
(4) Remote Procedure Call (RPC) - TechTarget. https://www.techtarget.com/searchapparchitecture/definition/Remote-Procedure-Call-RPC.

---

> Do you know about tRPC with Typescript? 

Yes, I know about tRPC with TypeScript. tRPC stands for TypeScript Remote Procedure Call, which is a library that allows you to easily build and consume fully typesafe APIs without schemas or code generation. It leverages the power of TypeScript to infer and apply your endpoints and their types on both the client and the server. It also supports various features such as subscriptions, request batching, and framework agnosticism¹.

tRPC is an alternative to traditional REST or GraphQL APIs, which require additional tools or steps to ensure typesafety across the network boundary. With tRPC, you can share types directly between your client and server, without relying on code generation or runtime overhead. tRPC is designed for full-stack TypeScript developers who want a simple and fast way to create typesafe APIs¹.

You can learn more about tRPC and how to use it with TypeScript in ¹, ², ³, ⁴, and ⁵.

Source: Conversation with Bing, 5/14/2023
(1) Intro to tRPC: Integrated, full-stack TypeScript | InfoWorld. https://www.infoworld.com/article/3690275/intro-to-trpc-integrated-full-stack-typescript.html.
(2) Build a full-stack TypeScript app using tRPC and React. https://blog.logrocket.com/build-full-stack-typescript-app-trpc-react/.
(3) tRPC | tRPC. https://trpc.io/docs/.
(4) Full-stack TypeScript with tRPC ft. Angular | AnyMind Group - Medium. https://medium.com/anymind-group/full-stack-typesscript-with-trpc-ft-angular-93ffc52685a5.
(5) Full-Stack TypeScript with tRPC and React - robinwieruch.de. https://www.robinwieruch.de/react-trpc/.

---
> What are the principles of operation for RPC ? 

The principles of operations for RPC are the following:

- RPC is a form of client-server interaction, where the client invokes a procedure or function on the server as if it were a local call, without explicitly writing the details for the remote communication¹.
- RPC uses a request-response message-passing system, where the client sends a request message containing the procedure name and parameters to the server, and the server sends back a response message containing the return value and any errors¹.
- RPC uses stubs, which are special instances that act as proxies for the remote procedure on both sides. The client stub packs the parameters into a message and sends it to the server stub, which unpacks the parameters and calls the actual procedure. The server stub then packs the return value into a message and sends it back to the client stub, which unpacks it and returns it to the client².
- RPC uses a low-level transport protocol, such as TCP/IP or UDP, to carry the message data between the client and the server. The transport protocol is responsible for ensuring reliable and efficient delivery of messages across the network³.
- RPC uses various mechanisms to handle issues such as naming, binding, marshalling, security, concurrency, fault tolerance, and performance. These mechanisms may vary depending on the implementation and specification of RPC¹.

You can learn more about the principles of operations for RPC in ¹, ², ³, and .

Source: Conversation with Bing, 5/14/2023
(1) Remote Procedure Call (RPC) - TechTarget. https://www.techtarget.com/searchapparchitecture/definition/Remote-Procedure-Call-RPC.
(2) Remote procedure call - Wikipedia. https://en.wikipedia.org/wiki/Remote_procedure_call.
(3) RPC (remote procedure call): What’s behind the technology ... - IONOS. https://www.ionos.com/digitalguide/server/know-how/what-is-a-remote-procedure-call/.
(4) Remote Procedure Call - IBM. https://www.ibm.com/docs/en/aix/7.1?topic=concepts-remote-procedure-call.

---


