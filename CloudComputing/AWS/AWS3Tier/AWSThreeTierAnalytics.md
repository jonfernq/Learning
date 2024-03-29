## AWS Three-Tier Application and Data Analytics Solution

**TABLE OF CONTENTS**
- Problem 
- Architecture Diagram
- Written Explanation of Architecture Diagram
- Justification for specific approach

### PROBLEM

You are working for a customer that runs their workloads on premises. Your customer has two workloads:

1. A three-tier architecture composed of a frontend (HTML, CSS, JavaScript), backend (Apache Web Server and a Java application), and database (MySQL). The three-tier application hosts a dynamic website that accepts user traffic from the internet.

2. A data analytics workload that runs Apache Hadoop. The analytics workload analyzes a massive amount of data that stored on premises and it also uses visualization tools to derive insights.

These components are currently running in the data center on physical servers. Currently, if a power outage occurred in the data center, all systems would be brought offline. Because of this issue (in addition to other benefits of the cloud), your customer wants to migrate all components to the cloud and, when possible, use AWS services to replace on-premises components.

You have been tasked with designing a solution that uses AWS services to decouple the application layers (frontend, backend, and database), and that hosts both the application and the data analytics workload in the cloud. 

Also, the data analytics solution currently runs on Hadoop and you have a requirement to spin up an Amazon EMR cluster for it. However, it’s up to you to choose which AWS services you want to use for the ingestion, storage, and visualization of data.

Use AWS managed services and advocate for refactoring the code to take advantage of cloud-native technologies. 

Write an explanation that details how the solution works and why you chose to use the services that you selected. Also, create an architecture diagram that depicts how both solutions will be hosted on AWS. 

---
### Architecture Diagram

The solution architecture follows a typical three-tier architecture pattern, where the web tier handles incoming requests and forwards them to the application tier, which further interacts with the database tier for data storage and retrieval. The use of load balancers, auto-scaling groups, and managed services like RDS, S3, EMR, and QuickSight helps ensure scalability, availability, and ease of management for the application.

![aws_diagram](https://github.com/jonfernq/Learning/assets/68504324/0b1fc807-a8a8-4370-a773-dcd5e98b44fa)

#### Written Explanation of Architecture Diagram

For the first part, we create a 3-tier architecture on AWS using the command line interface (CLI). This architecture consists of the web tier, application tier, and database tier.

In the web tier, we have two web instances that will handle the presentation of our web application. These instances will be launched in public subnets and will be load balanced using an Application Load Balancer. The load balancer will distribute incoming traffic evenly between the web instances, ensuring high availability and scalability.

Moving on to the application tier, we have two application instances that will handle the logic and processing of data received from the web tier. These instances will be launched in private subnets, which provide an additional layer of security by restricting direct access from the internet. The application instances will communicate with the web tier through the load balancer.

Finally, in the database tier, we have an RDS (Relational Database Service) instance that will store and manage our data. The RDS instance will also be launched in a private subnet to ensure secure access. The application instances from the application tier will interact with the database tier to retrieve and store data.

Explanation of the Solution:

1. Internet: This represents the external network or the internet from which requests originate.
    
2. Amazon Route 53: Route 53 is a scalable domain name system (DNS) web service provided by AWS. It routes the incoming requests from the internet to the appropriate services within the architecture based on the defined DNS configurations.
    
3. Application Load Balancer (ALB): The ALB is responsible for distributing incoming application traffic across multiple instances of the web tier. It performs advanced request routing and can handle HTTP and HTTPS traffic. It helps improve availability and scalability by distributing traffic evenly across healthy instances.
    
4. EC2 Auto Scaling Group (Frontend Instances): This group consists of multiple EC2 instances that host the frontend of the application. EC2 Auto Scaling helps automatically adjust the number of instances based on demand, ensuring high availability and scalability.
    
5. Elastic Beanstalk (Backend): (Optional, not added to diagram) Elastic Beanstalk is a platform-as-a-service (PaaS) offering by AWS. It provides an environment to deploy, manage, and scale applications. In this architecture, it hosts the backend instances, which handle the application logic and data processing.
    
6. Amazon RDS (MySQL): Amazon RDS is a managed relational database service by AWS. In this architecture, it hosts the MySQL database, providing a reliable and scalable storage solution for the application. The backend instances interact with the RDS instance to store and retrieve data.
    
7. Amazon S3 (Storage): Amazon S3 is an object storage service that provides scalable and durable storage for various types of data. It is often used for storing and retrieving large amounts of unstructured data, such as images, videos, and backups. In this architecture, Amazon S3 after the RDS instance is used for storing files alongside the database.
    
8. Amazon EMR (Hadoop): Amazon EMR is a managed big data platform that utilizes Apache Hadoop. It enables processing and analysis of large datasets using distributed computing.
    
9. Amazon QuickSight (Data Visualization): Amazon QuickSight is a business intelligence (BI) service enabling organizations to create interactive dashboards and visualizations to analyze data, generating insights and reports based on the data stored in RDS or EMR.
    
Two additional elements, an elastic cache tier before the RDS instance and an additional 'standby' instance after the RDS instance, are added to supplement the RDS instance in the database tier of the architecture that provide performance benefits:

1. **ElastiCache Tier**: Elasticache is an AWS managed service that provides an in-memory data store to improve the performance and scalability of applications. In this case, adding an ElastiCache tier before the RDS instance would involve using Amazon ElastiCache for caching frequently accessed data from the database.

Benefits:

* Improved Performance: ElastiCache reduces the latency and improves the response time of database queries by caching frequently accessed data in-memory. This reduces the load on the RDS instance, resulting in faster data retrieval.
* Scalability: ElastiCache is horizontally scalable, allowing you to add or remove cache nodes as needed to handle varying workloads. This helps to handle increased traffic without impacting the RDS instance's performance.
* Cost Optimization: By offloading read-intensive operations to ElastiCache, you can reduce the load on the RDS instance, potentially allowing you to use a smaller and more cost-effective RDS instance.

2. **Standby Instance**: Adding a standby instance after the RDS instance involves setting up a read replica or a Multi-AZ deployment for high availability and fault tolerance.

Benefits:

* High Availability: By setting up a standby instance using Multi-AZ or read replica, you ensure that there is a replica of the RDS instance that can take over automatically in case of a failure. This minimizes downtime and ensures continuity of operations.
* Fault Tolerance: With a standby instance, you have a redundant copy of your database that can handle failover seamlessly. This provides protection against infrastructure failures, ensuring your application remains accessible and operational.
* Load Distribution: If you configure your application to distribute read traffic across both the primary RDS instance and the standby instance, you can offload read operations from the primary instance, reducing the load and improving overall performance.

By adding an ElastiCache tier and a standby instance, you enhance the performance, scalability, and availability of your architecture. These additions help optimize the system's response time, handle increased traffic, and protect against failures, resulting in an improved user experience and system reliability.

---

### Analytics Workflow

The analytics workflow works briefly as follows. First, transfer data to Amazon S3 for storage, process data using Amazon EMR, and finally visualize the results using Amazon QuickSight. This enables one to leverage the power of big data analytics for deriving insights and making data-driven decisions. Here's a step-by-step more detailed explanation:

1. Data Extraction: SQL queries are used to extract data from the RDS database. These extracts are placed in S3 storage.  

2. Exporting to Amazon S3: Once SQL queries have extracted data from the RDS database, it is exported to Amazon S3 storage. Amazon S3 provides a reliable and scalable storage solution for large amounts of data. The AWS command-line interface (CLI) or SDKs can be used to transfer the data from your RDS instance to an S3 bucket. This can be done in various formats like CSV or JSON 
    
3. Data Processing with Amazon EMR: After the data is stored in Amazon S3, an Amazon EMR cluster is set up to process the data using distributed computing frameworks such as Apache Hadoop, Spark, or Presto. EMR simplifies the setup and management of big data infrastructure. You can define the processing steps and transformations you want to apply to the data using Hadoop-based tools like MapReduce or higher-level frameworks like Apache Spark. EMR reads the data from S3, performs the specified computations, and generates the desired results or derived datasets.
    
4. Storing Analyzed Data: Once the data processing is complete, the analyzed or transformed data can be stored back in Amazon S3 in a structured format providing easy access for further analysis or visualization.

5. Visualization with Amazon QuickSight: With the processed data available in Amazon S3, you can connect Amazon QuickSight to the S3 bucket and create interactive visualizations, dashboards, and reports. QuickSight provides a user-friendly interface for creating visualizations and exploring data. You can configure the data source in QuickSight to access the processed data stored in S3 and create visual representations of the insights derived from the data. 

---
### Justification for the specific approach

The decision was made to go cloud native with a complete end-to-end AWS solution 
instead of merely lifting and shifting.

Going cloud native for the three-tier web app combined with data analytics using EMR and QuickSight offers several advantages compared to simply lifting and shifting the on-premises workloads to the cloud. Here are the advantages of embracing cloud-native technologies and practices:

1. **Scalability and Elasticity**: Cloud-native architectures leverage the scalability and elasticity of cloud services. By adopting a cloud-native approach, you can easily scale your application resources up or down based on demand. For example, with AWS Auto Scaling, the web application frontend can automatically add or remove instances to handle traffic fluctuations. Additionally, EMR allows you to dynamically scale your data analytics cluster to process large datasets efficiently.
    
2. **Improved Resilience and Fault Tolerance**: Cloud-native architectures promote high availability and fault tolerance by utilizing managed services that are designed to be resilient. AWS services like Amazon RDS and Amazon S3 automatically handle replication and backups to ensure data durability. Moreover, leveraging multi-Availability Zone (AZ) deployments and managed services like Amazon Route 53 and Amazon ELB enhances the performance of the application.
    
3. **Cost Optimization**: Cloud-native architectures enable cost optimization by leveraging pay-as-you-go pricing models. With services like Amazon EC2 Auto Scaling and Elastic Beanstalk, you can scale resources based on demand, reducing unnecessary costs during periods of low traffic. Additionally, using managed services like Amazon RDS and Amazon EMR helps eliminate the need for upfront hardware investments and reduces maintenance costs.
    
4. **Enhanced Security**: Cloud providers like AWS offer security features and compliance certifications. By adopting cloud-native technologies, you can take advantage of built-in security measures like VPCs, security groups, and AWS Identity and Access Management (IAM). AWS services also undergo regular security audits and provide tools for monitoring and logging, allowing you to enhance the overall security posture of your application.
    
5. **Flexibility and Agility** (Devops): Cloud-native architectures enable faster development and deployment cycles. Infrastructure can be provisioned and configured programmatically using Infrastructure as Code (IaC) tools like AWS CloudFormation or AWS CDK. This allows for easier testing, iteration, and deployment of changes. Additionally, services like Elastic Beanstalk simplify the deployment process, reducing the time and effort required to bring updates and new features to production.
    
6. **Service Integration and Innovation**: Cloud-native architectures allow you to leverage a wide range of managed services and APIs provided by cloud providers. In the given architecture, services like Amazon EMR and QuickSight integrate seamlessly with other AWS offerings, providing powerful data analytics and visualization capabilities. These services allow you to take advantage of cutting-edge technologies and innovation without the need for extensive infrastructure management.
    
In summary, adopting a cloud-native approach offers numerous advantages over a lift-and-shift strategy. It provides scalability, resilience, cost optimization, security, flexibility, and integration with innovative services. By leveraging AWS managed services and embracing cloud-native technologies, you can build a highly scalable, resilient, and cost-effective solution.



