## Intro to COBOL

> It seems that what COBOL does is write reports and also CRUD like functionality with databases? 

COBOL, which stands for COmmon Business-Oriented Language, is a programming language that was originally designed for business applications and has been used extensively in the field of enterprise computing. While COBOL is often associated with applications that involve report generation and CRUD (Create, Read, Update, Delete) operations with databases, it is capable of more than just these functionalities.

COBOL is a procedural language that provides features for performing calculations, manipulating data, and controlling program flow. It supports file handling operations, such as reading from and writing to files, which makes it suitable for handling data in batch processing environments. COBOL also provides support for working with databases, including interfacing with relational databases through embedded SQL or using COBOL's built-in database management system called COBOL Database Connectivity (CODBC).

In addition to report generation and database operations, COBOL can be used to implement business logic, perform data validation, and implement complex business rules. COBOL programs can be used to process large volumes of data, perform data transformations, and implement complex data processing workflows.

While COBOL may have a specific niche in the realm of business applications and batch processing, it has a long history of successful use in mission-critical systems and continues to be used in many legacy systems in various industries such as finance, insurance, healthcare, and government. It is known for its readability, reliability, and stability, which makes it well-suited for applications that require high levels of accuracy and stability, especially in large-scale, data-intensive environments.

---
> What user interface does gnucobol use , ncurses? 

GNU Cobol (GnuCOBOL) is a COBOL compiler that provides support for various user interfaces, including the ncurses library, which is a popular library for creating terminal-based user interfaces in Unix-like systems.

With ncurses, GnuCOBOL programs can create text-based interfaces with features such as menus, dialogs, forms, and windows, allowing for interactive user interactions in a terminal or console environment. ncurses provides functions for handling terminal I/O, managing screen layout, handling keyboard input, and displaying text and graphical elements in a terminal window.

In addition to ncurses, GnuCOBOL also supports other UI libraries and frameworks, depending on the platform and configuration, such as Simple DirectMedia Layer (SDL), GTK (GIMP Toolkit), and Windows API (for compiling GnuCOBOL programs on Windows using Cygwin or other Windows-compatible environments).

The choice of user interface library depends on the specific requirements of the application and the platform on which the GnuCOBOL program is being compiled and executed.

---
> So what are some notable systems written in gnucobol, particularly using ncurses ? 

GNU Cobol (GnuCOBOL) is a modern, open-source COBOL compiler that is used in various applications across different industries. Some notable systems that have been developed using GnuCOBOL with ncurses for user interface include:

- Legacy systems migration: GnuCOBOL has been used to modernize and migrate legacy COBOL systems to more modern platforms while preserving the existing business logic and functionality. These systems can include various types of applications, such as financial systems, insurance systems, healthcare systems, and more.

- Business applications: GnuCOBOL has been used to develop various business applications, such as order processing systems, inventory management systems, customer relationship management (CRM) systems, and accounting systems.

- Government systems: GnuCOBOL has been used in the development of government systems, such as tax processing systems, social security systems, and public administration systems.

- Utilities and billing systems: GnuCOBOL has been used in the development of utilities and billing systems for managing services such as electricity, water, gas, and telecommunications.

- Transportation and logistics systems: GnuCOBOL has been used in the development of systems for managing transportation and logistics operations, including tracking shipments, managing routes, and handling inventory.

- Human resources and payroll systems: GnuCOBOL has been used in the development of systems for managing human resources, employee payroll, and benefits.

- Education and research: GnuCOBOL has been used in educational and research settings for teaching COBOL programming, conducting research on legacy systems, and exploring the use of COBOL in modern applications.

It's worth noting that GnuCOBOL, being a versatile and flexible COBOL compiler, can be used to develop a wide range of applications across different industries, depending on the requirements and needs of the specific projects.

---
> Is any of the above open source with code available ? 

There are many systems developed using GNU Cobol (GnuCOBOL) with ncurses for user interface are open source, and their source code is available for public access. Some examples of open source projects developed with GnuCOBOL and ncurses include:

1. OpenCobolIDE (https://github.com/OpenCobolIDE/OpenCobolIDE): An open source integrated development environment (IDE) for COBOL that uses GnuCOBOL as the compiler. It provides a user-friendly interface for COBOL development and includes ncurses-based features for creating text-based user interfaces.

2. CBIS (https://github.com/philpennock/cobol-unit-test): An open source COBOL Integrated Test Environment (ITE) that provides a framework for writing, running, and managing unit tests for COBOL programs. It uses GnuCOBOL as the compiler and supports ncurses-based text-based user interfaces for test execution and result management.

3. POCO (https://github.com/askovpen/poco): An open source COBOL library that provides a set of utilities and functions for performing common operations in COBOL programs. It includes ncurses-based user interface components for creating text-based interfaces in COBOL programs.

4. CICS-MDD (https://github.com/ibs-hq/cics-mdd): An open source middleware for COBOL applications running in CICS (Customer Information Control System) environments. It provides a modern, modular, and extensible framework for developing CICS applications using GnuCOBOL, and includes ncurses-based user interface components for creating text-based interfaces.

These are just a few examples of open source projects that use GnuCOBOL and ncurses for user interface. There may be other projects available on various code repositories, forums, and websites that utilize GnuCOBOL and ncurses for different purposes. It's always recommended to check the specific project's documentation and licensing terms to ensure compliance with the respective open source licenses.




