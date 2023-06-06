import 'dart:convert';
import 'dart:io';
import 'dart:ui';

import 'package:avm/Http/Dto/CalculatePriceResponseDto.dart';
import 'package:avm/Http/Dto/ProductDto.dart';
import 'package:avm/Http/Dto/TokenDto.dart';
import 'package:avm/Http/HttpError.dart';
import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

import 'package:avm/Util/constants.dart' as Constants;

import '../Http/Dto/CalculatePriceRequestDto.dart';

class ProductViewScreen extends StatefulWidget {
  const ProductViewScreen(this.product, {super.key});

  final ProductDto product;

  @override
  State<ProductViewScreen> createState() => _ProductViewScreenState(product);
}

class _ProductViewScreenState extends State<ProductViewScreen> {
  _ProductViewScreenState(this.product);

  final ProductDto product;
  final storage = const FlutterSecureStorage();

  List<TextEditingController> dimensionsValuesControllers =
      <TextEditingController>[];

  double price = 0;

  @override
  void initState() {
    super.initState();
    for (var i = 0; i < product.dimensions.length; i++) {
      dimensionsValuesControllers.add(TextEditingController());
    }
    calculatePrice();
  }

  @override
  void dispose() {
    for (var element in dimensionsValuesControllers) {
      element.dispose();
    }
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Visualizar Produto'),
      ),
      body: SingleChildScrollView(
        scrollDirection: Axis.vertical,
        child: Center(
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Padding(
                padding: const EdgeInsets.all(15),
                child: Text(
                  product.name,
                  style: const TextStyle(
                      fontSize: 14,
                      fontWeight: FontWeight.bold,
                      color: Colors.blue),
                ),
              ),
              Container(
                height: 300,
                child: ListView.builder(
                  scrollDirection: Axis.horizontal,
                  physics: const AlwaysScrollableScrollPhysics(),
                  itemCount: product.images.length,
                  itemBuilder: (context, index) {
                    return GestureDetector(
                      onTap: () {
                        debugPrint(
                            'imagem clicada: ${product.images[index].id}');
                      },
                      child: Card(
                        elevation: 5,
                        margin: EdgeInsets.all(10),
                        child: Image.network(product.images[index].url,
                            fit: BoxFit.fill),
                      ),
                    );
                  },
                ),
              ),
              product.dimensions.isNotEmpty ? const Padding(
                padding: EdgeInsets.all(15),
                child: Text(
                  'Dimensões',
                  style: TextStyle(
                    fontSize: 14,
                    fontWeight: FontWeight.bold,
                    color: Colors.blue,
                  ),
                )
              ) : const SizedBox(),
              SizedBox(
                height: 460 * product.dimensions.length.toDouble(),
                child: ListView.builder(
                  scrollDirection: Axis.vertical,
                  physics: const NeverScrollableScrollPhysics(),
                  itemCount: product.dimensions.length,
                  itemBuilder: (context, index) {
                    return Card(
                      elevation: 5,
                      margin: EdgeInsets.all(10),
                      child: Column(
                        children: [
                          Image.network(product.dimensions[index].images[0].url,
                              fit: BoxFit.fill),
                          Row(
                            children: [
                              Text('${product.dimensions[index].name}: '),
                              Expanded(
                                child: TextField(
                                  controller:
                                      dimensionsValuesControllers[index],
                                ),
                              ),
                            ],
                          ),
                        ],
                      ),
                    );
                  },
                ),
              ),
              product.dimensions.isNotEmpty ? Padding(
                padding: const EdgeInsets.all(15),
                child: ElevatedButton(
                  onPressed: calculatePrice,
                  child: const Text('Calcular preço'),
                ),
              ) : const SizedBox(),
              Padding(
                padding: const EdgeInsets.all(15),
                child: Text(
                  'Preço: ${price}',
                  style: const TextStyle(
                      fontSize: 14,
                      fontWeight: FontWeight.bold,
                      color: Colors.blue),
                ),
              ),
            ],
          ),
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

  Future<void> calculatePrice() async {
    var token = await storage.read(key: 'token');

    var url = Uri.http(Constants.apiEndpoint, '/products/calculatePrice');
    List<DimensionValueDto> values = <DimensionValueDto>[];
    for(var i = 0; i < product.dimensions.length; i++) {
      values.add(DimensionValueDto(product.dimensions[i].id, double.parse(dimensionsValuesControllers[i].text)));
    }
    CalculatePriceRequestDto request = CalculatePriceRequestDto(product.id, values);

    var response = await http.post(url, body: jsonEncode(request.toJson()), headers: {HttpHeaders.authorizationHeader: 'Bearer $token'});
    if (response.statusCode == 200) {
      var result = CalculatePriceResponseDto.fromJson(json.decode(response.body));
      setState(() {
        price = result.value;
      });
    } else {
      var error = HttpError.fromJson(json.decode(response.body));
      showErrorMessage(error.message);
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
}
