## Security 

> How you can make an operating system (OS) more secure? Think about files system, virtual memory, etc.

There are several ways to make an operating system more secure. Here are some of them:

1. Use USB security tokens (keys).
2. Use strong usernames and passwords.
3. Use biometric security (fingerprints, retina scanners).
4. Use multi-factor authentication.
5. Use virtualization.
6. Check file system regularly for consistency to guard against intrusion and tampering, this includes:

Integrity Verification Tools like Tripwire or AIDE (Advanced Intrusion Detection Environment) that compare file attributes, permissions, checksums, and/or digital signatures against a known good state to detect unauthorized changes.

Log Analysis: Examining system logs, access logs or event logs, for suspicious activities or file system modifications.

7. Use operating system hardening, reducing the OS 'surface of vulnerability' by performing fewer functions, reducing available ways of attack, changing default passwords, removal of unnecessary software, usernames, logins, network ports, removal of unnecessary services, automatic installation of security updates, setting up intrusion detection systems, firewalls and intrusion-prevention systems.


> Are there any aspects that you think will make your OS design more secure than today's OSes? Please explain.

The 'key security best practices' listed in Hailperin (2019:494-6) provide a helpful check list of often (if not usually) overlooked precautions:


- Consult with experts especially the website of the SANS (SysAdmin, Audit, Network, Security) Institute (http:// www.sans.org). 

- Adopt risk-management that looks at the security big picture, considering potential loss and adversary potential gain (and/or motives, such as those of disgruntled employees). [Personal anecdote: At the newspaper I worked at an outsourced IT personnel attempted to hack the line from editorial to printing facility to disrupt publication, and was subsequently prosecuted.]

- Deploy and properly configure firewalls to protect workgroup perimeters and important individual machines.

- Deploy multiple levels of anti-virus software, on servers as well as client machines.

- Keep software up-to-date.

- Employ system monitoring with an Intrusion Detection System (IDS), integrity checking software (Tripwire), and a secure logging platform (remote, incremental).

- Use end-to-end encryption (assuming all network communications are vulnerable)

- Use two-factor user authentication

- Maintain physical security over computer equipment.

- Stay on good terms with employees and to part from them cleanly.
- Cross-train employees and assign overlapping responsibilities.
- Eliminate need for solitary night or weekend work.
- Perform background checks.
- Set policies on acceptable use.
- Track security-relevant communications and check their origins (phone calls, emails frpm alleged vendors or law enforcement authorities)

- "Examine closely any case where the user whose authority is exercised by a process is not the same as the user who controls the process’s actions"

- "If at all possible, never run a program from an untrusted source. Failing that, run it with the least possible authority and the greatest possible monitoring."

- "If you need to write a setuid program, check very carefully what it does with all user input."

- "Examine any program that communicates over the network according to the exact same standards as a setuid program"

- Use secure coding practices.

Secure coding practices aim to mitigate vulnerabilities and ensure the robustness of software. Here is a summary of the practices mentioned:

Buffer-Overflow Prevention: Use secure programming languages, validate input, employ secure library functions, implement boundary checks, and keep software up to date.

Format-String Attack Prevention: Minimize user-controlled input in format string functions, validate and sanitize input, and consider safer alternatives.

Integer-Overflow Prevention: Choose appropriate variable types, perform range checks, use safe mathematical functions, and validate input to prevent overflow.

Path Traversal Prevention: Validate and sanitize user input, employ whitelisting, implement access controls and permission checks, and follow the principle of least privilege.


REFERENCES

Anderson, Ross. (2020) Security Engineering: A Guide to Building Dependable Distributed Systems. Third Edition. Wiley.

Hailperin, M. (2019). Operating systems and middleware: Supporting controlled interaction. Thomson Learning, Inc.: San Francisco, CA

Pfleeger, C.P., and S.L. Pfleeger. (2023) Security in Computing. Sixth Edition. Prentice Hall, 2015.

Wikipedia: Computer security, Secure coding, Security token, Hardening (computing), Passwords


