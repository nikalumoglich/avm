import 'package:avm/Http/Dto/ImageDto.dart';
import 'package:avm/Http/Dto/DimensionDto.dart';

class ProductDto {
  ProductDto(this.id, this.name, this.description, this.dimensions, this.images, this.priceFormula);

  int id;
  String name;
  String description;
  List<DimensionDto> dimensions;
  List<ImageDto> images;
  String priceFormula;

  ProductDto.fromJson(Map<String, dynamic> json)
      : id = json['productId'],
        name = json['name'],
        description = json['description'],
        dimensions = List<DimensionDto>.from(json['dimensions'].map((model)=> DimensionDto.fromJson(model))),
        images = List<ImageDto>.from(json['images'].map((model)=> ImageDto.fromJson(model))),
        priceFormula = json['priceFormula'];

  Map<String, dynamic> toJson() => {
    'productId': id,
    'name': name,
    'description': description,
    'dimensions': dimensions.map((model) => model.toJson()).toList(),
    'images': images.map((model) => model.toJson()).toList(),
    'priceFormula': priceFormula,
  };
}