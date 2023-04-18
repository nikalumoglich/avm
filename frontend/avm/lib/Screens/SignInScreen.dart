import 'dart:convert';

import 'package:avm/Http/Dto/TokenDto.dart';
import 'package:avm/Http/HttpError.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

import 'package:avm/Util/constants.dart' as Constants;
import 'package:avm/Http/Dto/SignInDto.dart';

class SignInScreen extends StatefulWidget {
  const SignInScreen({super.key});

  @override
  State<SignInScreen> createState() => _SignInScreenState();
}

class _SignInScreenState extends State<SignInScreen> {
  final emailController = TextEditingController();
  final passwordController = TextEditingController();
  final storage = const FlutterSecureStorage();

  @override
  void dispose() {
    emailController.dispose();
    passwordController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Sign In'),
      ),
      body: Center(
          child: Column(
            children: [
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
    SignInDto signinDto = SignInDto(emailController.text, passwordController.text);
    var url = Uri.http(Constants.apiEndpoint, '/signin');
    var response = await http.post(url, body: jsonEncode(signinDto.toJson()));
    if (response.statusCode == 200) {
      var tokenDto = Token.fromJson(json.decode(response.body));
      userLogged(tokenDto.token);
    } else {
      var error = HttpError.fromJson(json.decode(response.body));
      showErrorMessage(error.message);
    }
  }

  Future<void> userLogged(String token) async {
    showDialog(
      context: context,
      builder: (context) {
        return const AlertDialog(
          content: Text('Usuário logado'),
        );
      },
    );
    FocusManager.instance.primaryFocus?.unfocus();
    await Future.delayed(const Duration(seconds: 5));
    Navigator.pop(context);

    // to save token in local storage
    await storage.write(key: 'token', value: token);

    // to get token from local storage
    //var value = await storage.read(key: 'token');
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
