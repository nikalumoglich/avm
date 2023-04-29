import 'dart:convert';

import 'package:avm/Http/Dto/ImageDto.dart';

class DimensionDto {
  DimensionDto(this.id, this.images, this.name, this.productId, this.symbol);

  int id;
  List<ImageDto> images;
  String name;
  int productId;
  String symbol;

  DimensionDto.fromJson(Map<String, dynamic> json)
      : id = json['dimensionId'],
        images = List<ImageDto>.from(json['images'].map((model)=> ImageDto.fromJson(model))),
        name = json['name'],
        productId = json['productId'],
        symbol = json['symbol'];

  Map<String, dynamic> toJson() => {
    'dimensionId': id,
    'images': images.map((model) => model.toJson()).toList(),
    'name': name,
    'productId': productId,
    'symbol': symbol,
  };
}