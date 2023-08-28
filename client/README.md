# LSP Laboratory Client - VSCode Extension

This is the implementation of the VSCode client for our toy LSP server. Once
you build the server, you can use the client by running it (F5 or Run >
Debugging menu).

This was originally generated using `yo` and requires `npm` installed. The
important changes are to `package.json` which declares our toy language and
`src/extension.ts` which tells the extension how to connect to the server.