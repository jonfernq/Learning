## AWS System Design

**PROBLEM:**

You are working for a customer that runs their workloads on premises. Your customer has two workloads:

1. A three-tier architecture composed of a frontend (HTML, CSS, JavaScript), backend (Apache Web Server and a Java application), and database (MySQL). The three-tier application hosts a dynamic website that accepts user traffic from the internet.

2. A data analytics workload that runs Apache Hadoop. The analytics workload analyzes a massive amount of data that stored on premises and it also uses visualization tools to derive insights.

These components are currently running in the data center on physical servers. Currently, if a power outage occurred in the data center, all systems would be brought offline. Because of this issue (in addition to other benefits of the cloud), your customer wants to migrate all components to the cloud and, when possible, use AWS services to replace on-premises components.

You have been tasked with designing a solution that uses AWS services to decouple the application layers (frontend, backend, and database), and that hosts both the application and the data analytics workload in the cloud. 

Also, the data analytics solution currently runs on Hadoop and you have a requirement to spin up an Amazon EMR cluster for it. However, it’s up to you to choose which AWS services you want to use for the ingestion, storage, and visualization of data.

Use AWS managed services and advocate for refactoring the code to take advantage of cloud-native technologies. 

Write an explanation that details how the solution works and why you chose to use the services that you selected. Also, create an architecture diagram that depicts how both solutions will be hosted on AWS. 


---
**SOLUTION:** 

For the first part, we create a 3-tier architecture on AWS using the command line interface (CLI). This architecture consists of the web tier, application tier, and database tier.

In the web tier, we have two web instances that will handle the presentation of our web application. These instances will be launched in public subnets and will be load balanced using an Application Load Balancer. The load balancer will distribute incoming traffic evenly between the web instances, ensuring high availability and scalability.

Moving on to the application tier, we have two application instances that will handle the logic and processing of data received from the web tier. These instances will be launched in private subnets, which provide an additional layer of security by restricting direct access from the internet. The application instances will communicate with the web tier through the load balancer.

Finally, in the database tier, we have an RDS (Relational Database Service) instance that will store and manage our data. The RDS instance will also be launched in a private subnet to ensure secure access. The application instances from the application tier will interact with the database tier to retrieve and store data.

---
### Diagram:

A diagram of the AWS system architecture:

```scss
------------------------------------------
|               INTERNET                 |
------------------------------------------
                    |
             [Application Load Balancer]
                    |
------------------------------------------
|          WEB TIER (Public Subnets)     |
------------------------------------------
         |                      |
[Web Instance 1]        [Web Instance 2]
         |                      |
------------------------------------------
|      APPLICATION TIER (Private Subnets)|
------------------------------------------
         |                      |
[App Instance 1]        [App Instance 2]
         |                      |
------------------------------------------
|        DATABASE TIER (Private Subnets) |
------------------------------------------
                 |
           [RDS Instance]
```

In this diagram, the web tier consists of two instances (Web Instance 1 and Web Instance 2) running in public subnets. These instances are behind an Application Load Balancer, which distributes traffic to the instances.

The application tier consists of two instances (App Instance 1 and App Instance 2) running in private subnets. These instances process the data received from the web tier.

The database tier includes an RDS instance running in a private subnet. This is where the application tier interacts with the database.

Additional component details remain to be added such as security groups, routing tables, and NAT gateways, required to ensure proper network connectivity and security.

---
### AWS CLI Commands

For the first part, AWS CLI commands are used to set up this architecture via the AWS command line interface. These commands create virtual private cloud (VPC) resources, such as subnets and security groups, launch instances with a given configuration, and configure a load balancer and database instance.

Here's a step-by-step breakdown of the AWS CLI commands:

**WEB TIER:**

Step 1: Creating the VPC and subnets:

```shell
# Create VPC
aws ec2 create-vpc --cidr-block <vpc-cidr-block> --tag-specifications 'ResourceType=vpc,Tags=[{Key=Name,Value=<vpc-name>}]'

# Create public subnets
aws ec2 create-subnet --vpc-id <vpc-id> --cidr-block <public-subnet-cidr-block> --availability-zone <availability-zone> --tag-specifications 'ResourceType=subnet,Tags=[{Key=Name,Value=<public-subnet-name>}]'
aws ec2 create-subnet --vpc-id <vpc-id> --cidr-block <public-subnet-cidr-block> --availability-zone <availability-zone> --tag-specifications 'ResourceType=subnet,Tags=[{Key=Name,Value=<public-subnet-name>}]'

# Create private subnets
aws ec2 create-subnet --vpc-id <vpc-id> --cidr-block <private-subnet-cidr-block> --availability-zone <availability-zone> --tag-specifications 'ResourceType=subnet,Tags=[{Key=Name,Value=<private-subnet-name>}]'
aws ec2 create-subnet --vpc-id <vpc-id> --cidr-block <private-subnet-cidr-block> --availability-zone <availability-zone> --tag-specifications 'ResourceType=subnet,Tags=[{Key=Name,Value=<private-subnet-name>}]'
aws ec2 create-subnet --vpc-id <vpc-id> --cidr-block <private-subnet-cidr-block> --availability-zone <availability-zone> --tag-specifications 'ResourceType=subnet,Tags=[{Key=Name,Value=<private-subnet-name>}]'
aws ec2 create-subnet --vpc-id <vpc-id> --cidr-block <private-subnet-cidr-block> --availability-zone <availability-zone> --tag-specifications 'ResourceType=subnet,Tags=[{Key=Name,Value=<private-subnet-name>}]'

# Create NAT gateway
aws ec2 create-nat-gateway --subnet-id <public-subnet-id> --allocation-id <eip-allocation-id> --tag-specifications 'ResourceType=natgateway,Tags=[{Key=Name,Value=<nat-gateway-name>}]'
```

Step 2: Creating launch template:

```shell
# Create launch template
aws ec2 create-launch-template --launch-template-name <web-tier-launch-template-name> --launch-template-data file://web-tier-launch-template.json
```

Note: A JSON file named "web-tier-launch-template.json" is needed with the required launch template configuration.

Step 3: Create Auto Scaling Group

```shell
# Create auto scaling group
aws autoscaling create-auto-scaling-group --auto-scaling-group-name <web-tier-auto-scaling-group-name> --launch-template "LaunchTemplateName=<web-tier-launch-template-name>,Version=<version>" --min-size 2 --max-size 5 --target-group-arns <target-group-arn> --vpc-zone-identifier "<private-subnet-1>,<private-subnet-2>"
```

**APPLICATION TIER:**

Step 1: Create Application Launch Template

```shell
# Create launch template
aws ec2 create-launch-template --launch-template-name <application-tier-launch-template-name> --launch-template-data file://application-tier-launch-template.json
```

Note: A JSON file named "application-tier-launch-template.json" is needed with the required launch template configuration.

Step 2: Create Auto Scaling Group

```shell
# Create auto scaling group
aws autoscaling create-auto-scaling-group --auto-scaling-group-name <application-tier-auto-scaling-group-name> --launch-template "LaunchTemplateName=<application-tier-launch-template-name>,Version=<version>" --min-size 2 --max-size 5 --vpc-zone-identifier "<private-subnet-1>,<private-subnet-2>"
```

Note: Replace `<application-tier-auto-scaling-group-name>` with a desired name for the auto scaling group, `<application-tier-launch-template-name>` with the name of the application tier launch template created in the previous step, `<version>` with the version number of the launch template, and `<private-subnet-1>` and `<private-subnet-2>` with the IDs of the private subnets where you want to launch the application instances.

With these commands, a launch template for the application tier and an auto scaling group is created that will ensure the chosen application instances are running and properly scaled based on the specified minimum and maximum sizes.

Configure the launch template JSON file (`application-tier-launch-template.json`) with the necessary instance configurations, security groups, and other settings as per your requirements.

**DATABASE TIER:**

Here are the AWS CLI commands to complete the setup for the database tier:

Step 1: Creating a Database Instance

To create a database instance:

```shell
# Create database instance
aws rds create-db-instance --db-instance-identifier <db-instance-identifier> --db-instance-class <db-instance-class> --engine <engine> --allocated-storage <allocated-storage> --master-username <master-username> --master-user-password <master-user-password> --vpc-security-group-ids <vpc-security-group-ids> --availability-zone <availability-zone>
```

Note: Replace `<db-instance-identifier>` with a unique identifier for your database instance, `<db-instance-class>` with the desired instance class, `<engine>` with the database engine (e.g., "mysql" or "postgres"), `<allocated-storage>` with the amount of storage to allocate in GB, `<master-username>` and `<master-user-password>` with the credentials for the master user, `<vpc-security-group-ids>` with the ID of the security group associated with the database instance, and `<availability-zone>` with the desired availability zone.

Step 2: Create Read Replicas (optional)

To create read replicas for your database:

```shell
# Create read replica
aws rds create-db-instance-read-replica --db-instance-identifier <db-instance-identifier> --source-db-instance-identifier <source-db-instance-identifier> --availability-zone <availability-zone>
```

Note: Replace `<db-instance-identifier>` with a unique identifier for the read replica, `<source-db-instance-identifier>` with the identifier of the source database instance, and `<availability-zone>` with the desired availability zone.

Step 3: Configure Database Security Group

```shell
# Authorize inbound access to the database
aws ec2 authorize-security-group-ingress --group-id <database-security-group-id> --protocol tcp --port <port> --source-group <source-security-group>
```

Note: Replace `<database-security-group-id>` with the ID of the security group associated with the database instance, `<port>` with the port number used by your database engine (e.g., 3306 for MySQL), and `<source-security-group>` with the ID of the security group associated with your application instances.

With these commands, you can create the database instance, configure read replicas if necessary, and set up the necessary security group rules to allow inbound access to the database from your application instances. 
---
### SECOND PART: ANALYTICS ADDED

The architecture diagram below illustrates how both the three-tier application and the data analytics workload can be hosted on AWS:

```sql
                       +------------------------+
                       |                        |
                       |       Internet         |
                       |                        |
                       +-----------+------------+
                                   |
                                   |
                       +-----------v------------+
                       |                        |
                       |      Amazon Route 53    |
                       |                        |
                       +-----------+------------+
                                   |
                                   |
                       +-----------v------------+
                       |                        |
                       |    Application Load     |
                       |    Balancer (ALB)       |
                       |                        |
                       +-----------+------------+
                                   |
                  +----------------+----------------+
                  |                                 |
                  |       EC2 Auto Scaling Group     |
                  |      (Frontend Instances)       |
                  |                                 |
                  +----------------+----------------+
                                   |
                                   |
                  +----------------v----------------+
                  |                                 |
                  |   Elastic Beanstalk (Backend)   |
                  |                                 |
                  +----------------+----------------+
                                   |
                                   |
                  +----------------v----------------+
                  |                                 |
                  |      Amazon RDS (MySQL)         |
                  |                                 |
                  +----------------+----------------+
                                   |
                                   |
                  +----------------v----------------+
                  |                                 |
                  |       Amazon S3 (Storage)       |
                  |                                 |
                  +----------------+----------------+
                                   |
                                   |
                  +----------------v----------------+
                  |                                 |
                  |     Amazon EMR (Hadoop)        |
                  |                                 |
                  +----------------+----------------+
                                   |
                  +----------------v----------------+
                  |                                 |
                  |      Amazon QuickSight         |
                  |    (Data Visualization)        |
                  |                                 |
                  +---------------------------------+

```

Explanation of the Solution:

1. **Amazon Route 53**: We use Amazon Route 53 as the DNS service to route incoming traffic from the internet to our application. It provides domain registration and DNS management, ensuring high availability and fault tolerance.
    
2. **Application Load Balancer (ALB)**: The ALB acts as the entry point for the frontend layer of the three-tier architecture. It distributes incoming traffic across multiple EC2 instances in an Auto Scaling group to ensure scalability and fault tolerance.
    
3. **EC2 Auto Scaling Group**: The Auto Scaling group manages the frontend instances that host the HTML, CSS, and JavaScript components of the application. It automatically scales the number of instances based on demand, allowing the application to handle varying levels of user traffic.
    
4. **Elastic Beanstalk**: Elastic Beanstalk is used to deploy and manage the backend layer of the three-tier application. It supports various platforms, including Java, and automatically handles the deployment, capacity provisioning, load balancing, and health monitoring of the backend application.
    
5. **Amazon RDS (MySQL)**: We use Amazon RDS to host the MySQL database, which serves as the backend data store for the application. Amazon RDS provides a managed database service, ensuring high availability, durability, and automated backups.
    
6. **Amazon S3**: Amazon S3 serves as the storage solution for the data analytics workload. It provides highly scalable and durable object storage for the massive amount of data that needs to be ingested and processed by the analytics workload.
    
7. **Amazon EMR**: Amazon EMR is used to run the Apache Hadoop-based data analytics workload. EMR provides a managed cluster platform for big data processing, allowing us to spin up Hadoop clusters on-demand. By leveraging EMR, we can take advantage of its scalability, fault tolerance, and integration with other AWS services.
    
8. **Amazon QuickSight**: QuickSight

---
AWS CLI commands to build the above system.

To build the system described above using AWS CLI commands, you would need to execute a series of commands for each component. Here's a step-by-step guide:

1. Create an Application Load Balancer (ALB):

```shell
aws elbv2 create-load-balancer --name my-application-alb --subnets subnet-1a123456 subnet-1b789012 --security-groups sg-12345678
```

2. Create a target group for the ALB:

```shell
aws elbv2 create-target-group --name my-target-group --protocol HTTP --port 80 --target-type instance --vpc-id vpc-12345678
```

3. Create an Auto Scaling group:

```shell
aws autoscaling create-auto-scaling-group --auto-scaling-group-name my-auto-scaling-group --launch-configuration-name my-launch-config --target-group-arns arn:aws:elasticloadbalancing:us-west-2:123456789012:targetgroup/my-target-group/abcdefg --min-size 2 --max-size 5 --desired-capacity 2 --vpc-zone-identifier subnet-1a123456 subnet-1b789012 --health-check-type ELB
```

4. Create an Elastic Beanstalk environment:

```shell
aws elasticbeanstalk create-environment --application-name my-application --environment-name my-environment --solution-stack-name "64bit Amazon Linux 2 v5.3.1 running Java 11" --option-settings Namespace=aws:autoscaling:launchconfiguration,OptionName=InstanceType,Value=t2.micro
```

5. Create an Amazon RDS database instance:

```shell
aws rds create-db-instance --db-instance-identifier my-db-instance --db-instance-class db.t2.micro --engine MySQL --master-username myuser --master-user-password mypassword --allocated-storage 20 --vpc-security-group-ids sg-12345678 --availability-zone us-west-2a
```

6. Create an Amazon S3 bucket:

```shell
aws s3api create-bucket --bucket my-bucket --region us-west-2
```

7. Create an Amazon EMR cluster:

```shell
aws emr create-cluster --name my-emr-cluster --release-label emr-6.5.0 --applications Name=Hadoop Name=Hive --ec2-attributes KeyName=my-key-pair --instance-type m5.xlarge --instance-count 3 --use-default-roles
```

Once you have executed these commands, you will have the foundational components set up for your system. Remember to replace the placeholder values (e.g., subnet-1a123456, sg-12345678) with the appropriate values for your specific configuration.

---
**REFERENCES**

Amazon Web Services (2021). **AWS Serverless Multi-Tier Architectures With Amazon API Gateway and AWS Lambda.** Amazon Web Services (AWS). https://d0.awsstatic.com/whitepapers/AWS_Serverless_Multi-Tier_Architectures.pdf

Medium (2023, January 19). "How to Build a 3 Tier Architecture in AWS. AWS in Plain English." Retrieved June 25, 2023, from https://aws.plainenglish.io/

Shrivastava S. Srivastav N. Artasanchez A. Sayed I. & Choubey S. (2023). *Aws for solutions architects : the definitive guide to aws solutions architecture for migrating to building scaling and succeeding in the cloud* (Second Edition). Packt Publishing. Retrieved June 24 2023 from https://www.oreilly.com/library/view/-/9781803238951/.





