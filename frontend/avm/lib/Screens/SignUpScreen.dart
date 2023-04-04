import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;

import 'package:avm/Http/Dto/SignUpDto.dart';

class SignUpScreen extends StatefulWidget {
  const SignUpScreen({super.key});

  @override
  State<SignUpScreen> createState() => _SignUpScreenState();
}

class _SignUpScreenState extends State<SignUpScreen> {
  final nameController = TextEditingController();
  final emailController = TextEditingController();
  final passwordController = TextEditingController();

  @override
  void dispose() {
    nameController.dispose();
    emailController.dispose();
    passwordController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Sign Up'),
      ),
      body: Center(
        child: Column(
          children: [
            Row(
              children: [
                const Text('Nome: '),
                Expanded(child:
                  TextField(
                    controller: nameController,
                  ),
                ),
              ],
            ),
            Row(
              children: [
                const Text('E-mail: '),
                Expanded(child:
                  TextField(
                    controller: emailController,
                  ),
                ),
              ],
            ),
            Row(
              children: [
                const Text('Senha: '),
                Expanded(child:
                  TextField(
                    controller: passwordController,
                    obscureText: true,
                    enableSuggestions: false,
                    autocorrect: false,
                  ),
                ),
              ],
            ),
            ElevatedButton(
              child: const Text('Sign Up'),
              onPressed: sendData,
            ),
          ],
        )
      ),
    );
  }

  Future<void> sendData() async {
    SignUpDto signupDto = SignUpDto(nameController.text, emailController.text, passwordController.text);
    var url = Uri.http('192.168.15.17:3000', '/signup');
    var response = await http.post(url, body: jsonEncode(signupDto.toJson()));
    userCreated(response.body);
  }

  void userCreated(String result) {
    showDialog(
      context: context,
      builder: (context) {
        return AlertDialog(
          content: Text('Result: $result'),
        );
      },
    );
  }
}
