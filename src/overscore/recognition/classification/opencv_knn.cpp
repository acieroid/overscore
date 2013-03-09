#include <cv.h>
#include <ml.h>

/**
 * Read the training set from stdin, then, for each vector given in
 * stdin, output the corresponding class
 *
 * To build:
 * g++ opencv_knn.cpp -o opencv_knn `pkg-config opencv --libs --cflags`
 */
int main(int argc, char *argv[])
{
  int k;
  int trainingSetSize; /* the size of the training set */
  int size;            /* the sizes of the vectors (400 for 20x20 images) */
  std::cin >> k >> trainingSetSize >> size;

  CvMat *trainingSet = cvCreateMat(trainingSetSize, size, CV_32FC1);
  CvMat *labels = cvCreateMat(trainingSetSize, 1, CV_32FC1);

  /* read the training set */
  for (int i = 0; i < trainingSetSize; i++) {
    /* read the label from stdin */
    std::cin >> labels->data.fl[i];

    /* read the vector from stdin */
    for (int j = 0; j < size; j++) {
      std::cin >> trainingSet->data.fl[i*size+j];
    }
  }
      
  cv::KNearest knn(trainingSet, labels);

  cvReleaseMat(&trainingSet);
  cvReleaseMat(&labels);

  /* read and classify the given data */
  CvMat *vec = cvCreateMat(1, size, CV_32FC1);
  CvMat *label = cvCreateMat(1, 1, CV_32FC1);
  while (!std::cin.eof()) {
    /* read the given vector */
    for (int j = 0; j < size; j++) {
      if (std::cin.eof()) {
        goto end;
      }
      std::cin >> vec->data.fl[j];
    }

    /* classify */
    knn.find_nearest(vec, k, label);

    std::cout << label->data.fl[0] << std::endl;
  }

 end:
  cvReleaseMat(&vec);
  cvReleaseMat(&label);

  return 0;
}
