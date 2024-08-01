terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.16"
    }
  }
}

# Configure the AWS Provider
provider "aws" {
  region = "sa-east-1"
}

# Create a VPC
resource "aws_default_vpc" "default_vpc" {
}

resource "aws_default_subnet" "default_subnet_a" {
  # Use your own region here but reference to subnet 1a
  availability_zone = "sa-east-1a"
}

resource "aws_default_subnet" "default_subnet_b" {
  # Use your own region here but reference to subnet 1b
  availability_zone = "sa-east-1b"
}

resource "aws_subnet" "private_subnet" {
  vpc_id                  = "${aws_default_vpc.default_vpc.id}"
  map_public_ip_on_launch = false
}

resource "aws_ecr_repository" "avm_ecr_repo" {
  name = "avm"
  image_scanning_configuration {
    scan_on_push = true
  }
}

resource "aws_s3_bucket" "avm_bucket" {
  bucket = "avm-piloto"

  tags = {
    Name        = "Avm main bucket"
    Environment = "Dev"
  }
}

data "aws_iam_policy_document" "assume_role" {
  statement {
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["eks.amazonaws.com"]
    }

    actions = ["sts:AssumeRole"]
  }
}

resource "aws_iam_role" "avm-eks" {
  name               = "avm-eks"
  assume_role_policy = data.aws_iam_policy_document.assume_role.json
}

resource "aws_iam_role_policy_attachment" "avm_eks-AmazonEKSClusterPolicy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
  role       = aws_iam_role.avm-eks.name
}

resource "aws_iam_role_policy_attachment" "avm_eks-AmazonEKSVPCResourceController" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSVPCResourceController"
  role       = aws_iam_role.avm-eks.name
}

resource "aws_eks_cluster" "avm_cluster" {
  name     = "avm"
  role_arn = aws_iam_role.avm-eks.arn

  vpc_config {
    subnet_ids = [aws_default_subnet.default_subnet_a.id, aws_default_subnet.default_subnet_b.id]
  }

  # Ensure that IAM Role permissions are created before and deleted after EKS Cluster handling.
  # Otherwise, EKS will not be able to properly delete EKS managed EC2 infrastructure such as Security Groups.
  depends_on = [
    aws_iam_role_policy_attachment.avm_eks-AmazonEKSClusterPolicy,
    aws_iam_role_policy_attachment.avm_eks-AmazonEKSVPCResourceController,
  ]
}

output "endpoint" {
  value = aws_eks_cluster.avm_cluster.endpoint
}

output "kubeconfig-certificate-authority-data" {
  value = aws_eks_cluster.avm_cluster.certificate_authority[0].data
}






resource "aws_eks_fargate_profile" "default" {
  cluster_name           = "avm"
  fargate_profile_name   = "default"
  pod_execution_role_arn = aws_iam_role.eks_fargate_profile_role.arn
  subnet_ids             = [aws_subnet.private_subnet.id]

  selector {
    namespace = "kube-system"
  }

  selector {
    namespace = "default"
  }
}

resource "aws_iam_role" "eks_fargate_profile_role" {
  name = "eks-fargate-profile-role"

  assume_role_policy = jsonencode({
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "eks-fargate-pods.amazonaws.com"
      }
    }]
    Version = "2012-10-17"
  })
}

resource "aws_iam_role_policy_attachment" "AmazonEKSFargatePodExecutionRolePolicy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSFargatePodExecutionRolePolicy"
  role       = aws_iam_role.eks_fargate_profile_role.name
}
