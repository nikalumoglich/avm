import 'dart:convert';
import 'dart:io';
import 'dart:ui';

import 'package:avm/Http/Dto/ProductDto.dart';
import 'package:avm/Http/Dto/TokenDto.dart';
import 'package:avm/Http/HttpError.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

import 'package:avm/Util/constants.dart' as Constants;

import 'ProductViewScreen.dart';

class ProductsScreen extends StatefulWidget {
  const ProductsScreen({super.key});

  @override
  State<ProductsScreen> createState() => _ProductsScreenState();
}

class _ProductsScreenState extends State<ProductsScreen> {
  final storage = const FlutterSecureStorage();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Lista de Produtos'),
      ),
      body: Center(
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            const Padding(
              padding: EdgeInsets.all(15),
              child: Text(
                "Lista de produtos",
                style: TextStyle(
                    fontSize: 14,
                    fontWeight: FontWeight.bold,
                    color: Colors.blue),
              ),
            ),
            Expanded(
              child: FutureBuilder<List<ProductDto>>(
                future: loadData(),
                builder: (context, snapshot) {
                  if (snapshot.hasData) {
                    return Container(
                      child: ListView.builder(
                        scrollDirection: Axis.vertical,
                        physics: const AlwaysScrollableScrollPhysics(),
                        itemCount: snapshot.data!.length,
                        itemBuilder: (context, index) {
                          ProductDto model = snapshot.data![index];
                          return GestureDetector(
                            onTap: () {
                              loadProductView(model);
                            },
                            child: Card(
                              elevation: 5,
                              margin: EdgeInsets.all(10),
                              child: Column(
                                mainAxisSize: MainAxisSize.min,
                                children: [
                                  Image.network(model.images![0].url.toString(),
                                      fit: BoxFit.fill),
                                  Padding(
                                    padding: EdgeInsets.all(15),
                                    child: Text(
                                      model.name,
                                      style: const TextStyle(
                                          fontSize: 14,
                                          fontWeight: FontWeight.bold,
                                          color: Colors.blue),
                                    ),
                                  ),
                                ],
                              ),
                            ),
                          );
                        },
                      ),
                    );
                  } else if (snapshot.hasError) {
                    return Text('${snapshot.error}');
                  }
                  return const CircularProgressIndicator();
                },
              ),
            ),
          ],
        ),
      ),
    );
  }

  Future<List<ProductDto>> loadData() async {
    var token = await storage.read(key: 'token');

    var url = Uri.http(Constants.apiEndpoint, '/products');
    var response = await http
        .get(url, headers: {HttpHeaders.authorizationHeader: 'Bearer $token'});
    if (response.statusCode == 200) {
      List<ProductDto> products = List<ProductDto>.from(json
          .decode(response.body)
          .map((model) => ProductDto.fromJson(model)));
      return products;
    } else {
      var error = HttpError.fromJson(json.decode(response.body));
      showErrorMessage(error.message);
      throw Exception('Failed to load product list');
    }
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

  void loadProductView(ProductDto product) {
    Navigator.of(context).push(MaterialPageRoute(builder: (context) => ProductViewScreen(product)));
  }
}
