// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import { execSync } from 'child_process';
import path = require('path');
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | null = null;
let outputChannel: vscode.OutputChannel;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	outputChannel = vscode.window.createOutputChannel('LSP Laboratory');
	outputChannel.show(true);
	outputChannel.appendLine('Activating "lsp-lab-client"...');

	let stdout: Buffer;
	try {
		stdout = execSync(`find ${__dirname}/../../server -iname lsp-lab-server-exe -type file | tail -n1`);
	} catch {
		outputChannel.appendLine("There was a problem finding the LSP server executable.");
		return;
	}

	const execPath = path.normalize(stdout.toString('utf-8').replace(/\n+$/, ''));
	outputChannel.appendLine(`LSP server executable path: "${execPath}"`);

	const serverOptions: ServerOptions = {
		run: {
			command: execPath,
			transport: TransportKind.stdio,
			args: [],
		},
		debug: {
			command: execPath,
			transport: TransportKind.stdio,
			args: [],
		},
	};

	const clientOptions: LanguageClientOptions = {
		// Register the server for While documents
		documentSelector: [{ scheme: 'file', language: 'LSPLab' }],
		outputChannel: outputChannel,
	};

	client = new LanguageClient(
		'lsp-lab-client',
		'LSP Laboratory',
		serverOptions,
		clientOptions
	);

	client.start();
}

// This method is called when your extension is deactivated
export function deactivate() {
	if (client) {
		client.stop();
	}
}
