## Databases 

### Relational Databases

- [Joins](https://github.com/jonfernq/Learning/blob/main/Databases/Joins.md)
- [Grouping and Aggregrates](https://github.com/jonfernq/Learning/blob/main/Databases/GroupingAndAggregates.md) 

### MS Access Open Source Lookalikes

More useful info from the lil' oracle, if you can imagine something that should exist in the open source world, it probably does, namely here MS Access lookalikes that use SQLite: 

https://github.com/giowck/symphytum
https://www.osalt.com/glom

---
> Open source LibreOffice and OpenOffice do not use SQLite? I would expect them to, why not? 

Open source office suites like LibreOffice and OpenOffice do not use SQLite as their primary database engine, but rather rely on other technologies for their data storage needs.

One reason for this is that SQLite is primarily a relational database engine, which means it's optimized for handling structured data in tables with defined relationships between them. While this makes SQLite a good choice for many applications, it may not be the best fit for applications like office suites that need to handle a wide variety of data types and formats.

In addition, SQLite is designed to be embedded within an application rather than running as a separate database server, which means it may not be as well-suited to handling large, complex datasets as other database engines that are designed for server-based deployments.

Instead of SQLite, LibreOffice and OpenOffice use their own built-in database engines or rely on other open source database technologies like HSQLDB or Firebird. These database engines are optimized for handling the types of data and document formats that are commonly used in office suites, and they integrate closely with the rest of the application to provide a seamless user experience.

---
> But LibreOffice 'Base' is basically the open source equivalent of Microsoft Access which is a relational database, so it would make sense for Base to use SQLite, right? Is there some other open source Access-like offering that uses SQLite?  

You are correct that LibreOffice's Base module is a relational database management system that provides a graphical interface for designing database schemas and managing data. While Base supports a variety of database engines, including HSQLDB and Firebird, it does not use SQLite as its primary database engine.

However, there are other open source database management systems that are similar to Access and do use SQLite as their underlying database engine. One example is called "Symphytum," which is a lightweight, cross-platform database management system that uses SQLite as its database engine. Symphytum is designed to be easy to use and provides a graphical interface for designing and managing database schemas, as well as querying and reporting on data.

Another option is "Glom," which is a more feature-rich open source database management system that uses SQLite as well as other database engines like PostgreSQL and MySQL. Glom is designed to be more flexible and customizable than Symphytum, and provides advanced features like customizable forms, scripting support, and integration with external applications.

Both of these systems are relatively easy to use and provide similar functionality to Access, while using SQLite as their underlying database engine.


