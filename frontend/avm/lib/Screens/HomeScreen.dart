import 'package:flutter/material.dart';
import 'package:avm/Screens/SignUpScreen.dart';
import 'package:avm/Screens/SignInScreen.dart';

class HomeScreen extends StatelessWidget {
  const HomeScreen({super.key});

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Home Screen'),
      ),
      body: Center(
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.center,
          mainAxisAlignment: MainAxisAlignment.end,
          children: [
            SizedBox(
              width: 200,
              child: ElevatedButton(
                child: const Text('Sign In'),
                onPressed: () {
                  Navigator.push(
                    context,
                    MaterialPageRoute(builder: (context) => const SignInScreen()),
                  );
                },
              ),
            ),
            SizedBox(
              width: 200,
              child: ElevatedButton(
                child: const Text('Sign Up'),
                onPressed: () {
                  Navigator.push(
                    context,
                    MaterialPageRoute(builder: (context) => const SignUpScreen()),
                  );
                },
              ),
            ),
            const SizedBox(
              height: 20,
            )
          ],
        )
      ),
    );
  }
}
