import 'package:flutter/material.dart';

class SignInScreen extends StatelessWidget {
  const SignInScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Sign In'),
      ),
      body: Center(
        child: ElevatedButton(
          child: const Text('Login'),
          onPressed: () {
            /*Navigator.push(
              context,
              MaterialPageRoute(builder: (context) => const SecondRoute()),
            );*/
          },
        ),
      ),
    );
  }
}
