import 'dart:convert';

import 'package:avm/Http/HttpError.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;

import 'package:avm/Util/constants.dart' as Constants;
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
    var url = Uri.http(Constants.apiEndpoint, '/signup');
    var response = await http.post(url, body: jsonEncode(signupDto.toJson()));
    if (response.statusCode == 200) {
      userCreated();
    } else {
      var error = HttpError.fromJson(json.decode(response.body));
      showErrorMessage(error.message);
    }
  }

  Future<void> userCreated() async {
    showDialog(
      context: context,
      builder: (context) {
        return const AlertDialog(
          content: Text('Usuário criado'),
        );
      },
    );
    FocusManager.instance.primaryFocus?.unfocus();
    //await Future.delayed(const Duration(seconds: 5));
    //Navigator.pop(context);
    //Navigator.pop(context);
  }

  void showErrorMessage(String errorMessage) {
    showDialog(
      context: context,
      builder: (context) {
        return AlertDialog(
          content: Text(errorMessage),
        );
      },
    );
  }
}
