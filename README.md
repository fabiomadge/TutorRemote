This tool is meant to improve the overall experience in the practical part of my tutorials, by enabling the students to remotely use my computers keyboard.

##Model
The communication protocol is build on top of web sockets. The JS Clients send keyboard events, or strings of text, to the Haskell server, which relays them to to all Mac clients. On the Mac the messages are turned into *KeyboardEvents*, or just pasted.