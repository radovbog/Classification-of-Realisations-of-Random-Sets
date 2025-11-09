
/*
*      CERTAIN PARTS OF THIS CODE WERE KINDLY PROVIDED BY KATERINA BREJCHOVA
*      AND CAN BE FOUND AT 
*      https://gitlab.com/katerinab/random-sets-helper-programs
*      LICENCED UNDER THE MIT LICENCE
*      Copyright (c) 2020 Katerina Brejchova
*/


/*
* THIS PROGRAM IS A MODIFIED VERSION OF THE curvature_ratio.cpp 
* USED FOR WORKING WITH DATA OF MAMMARY TISSUE
*/


#include <iostream>
#include <stack>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <numeric>

// Size of one side of the square bitmap
#define MAX_SIZE_x 512
#define MAX_SIZE_y 5120 //4608 for picture No. 4

// Value in range (0, 1), i.e. set 0.6 to delete component with probability 60%
#define DELETE_PROBABILITY 0

// Do not change, unless you want to delete some components
#define UNASSIGNED_COMPONENT 0

// Radius and area of a measuring circle
#define B_SIZE 3    //5
#define B_AREA 29   //81
using namespace std;

struct Coordinates {
  int x;
  int y;
  int curvature;
  int comp_id;
};

/*
 * Returns true if the coordinate is inside of bitmap.
 */
bool is_valid_coordinates(Coordinates coordinates) {
  return coordinates.x >= 0 && coordinates.x < MAX_SIZE_x && coordinates.y >= 0 &&
         coordinates.y < MAX_SIZE_y;
}

bool is_same_component(int id, Coordinates pos){
return id==pos.comp_id;
}
/*
 * Returns whether the current component should be deleted based on given probability.
 */
bool should_delete_random() {
  return ((double)rand() / RAND_MAX) < DELETE_PROBABILITY;
}

/*
 * Returns true if at least one of the 4-neighbors is not in the set (=0).
 */
bool is_on_edge(Coordinates cur_position, bool bitmap[MAX_SIZE_x][MAX_SIZE_y]) {
  Coordinates up = {cur_position.x, cur_position.y - 1};
  Coordinates down = {cur_position.x, cur_position.y + 1};
  Coordinates right = {cur_position.x + 1, cur_position.y};
  Coordinates left = {cur_position.x - 1, cur_position.y};

  if (!is_valid_coordinates(up) || !bitmap[up.x][up.y])
    return true;
  if (!is_valid_coordinates(down) || !bitmap[down.x][down.y])
    return true;
  if (!is_valid_coordinates(left) || !bitmap[left.x][left.y])
    return true;
  if (!is_valid_coordinates(right) || !bitmap[right.x][right.y])
    return true;
  return false;
}

/*
 * Returns number of pixels in the b_size-surroundings of given pixel.
 */
int get_curvature(bool bitmap[MAX_SIZE_x][MAX_SIZE_y], Coordinates pos, int b_size, int comp[MAX_SIZE_x][MAX_SIZE_y]) {
  float distance;
  int curvature = 0;
  float num_to_average = 0.0;

  for (int x = pos.x - b_size; x <= pos.x + b_size; x++) {
    for (int y = pos.y - b_size; y <= pos.y + b_size; y++) {
      distance = sqrt(pow(pos.x - x, 2) + pow(pos.y - y, 2));
      if (is_valid_coordinates({x, y}) && distance <= b_size && distance > 0 && comp[x][y]==pos.comp_id) {
          curvature += bitmap[x][y];
          num_to_average += 1;
        }
      }
    }
  return curvature;
}

/*
 * Returns all valid neighbors from 4-neighborhood of given coordinate.
 */
std::vector<Coordinates> get_neighbors(Coordinates cur_position) {
  std::vector<Coordinates> neighbors;
  Coordinates up = {cur_position.x, cur_position.y - 1};
  Coordinates down = {cur_position.x, cur_position.y + 1};
  Coordinates right = {cur_position.x + 1, cur_position.y};
  Coordinates left = {cur_position.x - 1, cur_position.y};

  if (is_valid_coordinates(up))
    neighbors.push_back(up);
  if (is_valid_coordinates(down))
    neighbors.push_back(down);
  if (is_valid_coordinates(left))
    neighbors.push_back(left);
  if (is_valid_coordinates(right))
    neighbors.push_back(right);
  return neighbors;
}

/*
 * Runs DFS to mark all bits belonging to the same component as cur_position.
 */
void dfs_components(Coordinates cur_position, int component_id,
                    bool pixels[MAX_SIZE_x][MAX_SIZE_y], int components[MAX_SIZE_x][MAX_SIZE_y]) {
  std::stack<Coordinates> stack;
  Coordinates q_position;
  stack.push(cur_position);

  
  components[cur_position.x][cur_position.y] = component_id;

  while (!stack.empty()) {
    q_position = stack.top();
    stack.pop();
    for (Coordinates neighbor : get_neighbors(q_position)) {
      if (pixels[neighbor.x][neighbor.y] &&
          components[neighbor.x][neighbor.y] == UNASSIGNED_COMPONENT) {
        stack.push(neighbor);
        
        components[neighbor.x][neighbor.y] = component_id;
      }
    }
  }
}


/*
 * Prints curvature of border points in a form of:
    border_point[0].comp_id border_point[0].curvature
    border_point[1].comp_id border_point[1].curvature
    ...
    border_point[|border_points|].comp_id border_point[|border_points|].curvature
    empty line
 */


void print_curvature(std::vector<Coordinates> curvature_coords) {
  for (Coordinates coo : curvature_coords) {
    printf("%d %d\n", coo.comp_id, coo.curvature);
  }
  printf("\n");

}

/*
*Sorts out vector of Coordinates in ascending order of size
*/
bool sort_coordinates(Coordinates &c1, Coordinates &c2) {
    if (c1.comp_id == c2.comp_id)
        return c1.curvature < c2.curvature;
    return c1.comp_id < c2.comp_id;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////  M A I N  ///////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////
static int components[MAX_SIZE_x][MAX_SIZE_y] = {};     // Initializes all elements to 0
static int pixels_final[MAX_SIZE_x][MAX_SIZE_y]= {};    // To save the bitmap after enumerating
static bool pixels[MAX_SIZE_x][MAX_SIZE_y] = {};        // To save the bitmap

int main(int argc, char const *argv[]) {
    int component_id = 1;
    int value;
    int value_final;
    int total_border_length;
    int curvature_old = 0;
    int curvature;
    int comp_id_max;
    int circle_size =29;
    std::vector<Coordinates> curvature_coords;
    std::vector<int> curvature_ids;
    std::vector<int> curvature_per_comp;
    std::vector<int> num_border_pixels;
    std::vector<Coordinates> curvature_coord;

    // LOAD DATA
    for (int x = 0; x < MAX_SIZE_x; ++x) {
        for (int y = 0; y < MAX_SIZE_y; y++) {
            scanf("%d", &value);
            pixels[x][y] = value;
        }
    }


    // FIND AND *DELETE* COMPONENTS (note that DELETE_PROBABILITY is set to 0)
    for (int x = 0; x < MAX_SIZE_x; ++x) {
        for (int y = 0; y < MAX_SIZE_y; y++) {
            if (pixels[x][y] && components[x][y] == UNASSIGNED_COMPONENT) {
                dfs_components({x, y}, component_id, pixels, components);
                component_id++;
            }
        }
    }


    // SCAN DFS_COMPONENTS
    for (int x = 0; x < MAX_SIZE_x; ++x) {
        for (int y = 0; y < MAX_SIZE_y; y++) {
            curvature_coord.push_back({x, y, curvature_old, components[x][y]});
        }
    }


    // GET CURVATURE
    for (int x = 0; x < MAX_SIZE_x; ++x) {
        for (int y = 0; y < MAX_SIZE_y; y++) {
            if (pixels[x][y] && is_on_edge({x, y}, pixels)) {
                curvature = get_curvature(pixels, curvature_coord.at(x*MAX_SIZE_y + y), B_SIZE, components);
                curvature_coords.push_back({x, y, curvature, components[x][y]});
            }
        }
    }


    // SORT
    sort(curvature_coords.begin(), curvature_coords.end(), sort_coordinates);


    comp_id_max = component_id -1;                  // Number of components in bitmap
    total_border_length = curvature_coords.size();  // Number of border pixels in bitmap  


    // STORES NUMBER OF BORDER PIXELS FOR EACH COMPONENT ON SPECIFIED POSITION
    vector<int> border_count;
        for (int j = 1; j <= comp_id_max; ++j){ // There is no component with comp_id = 0
            int bc =0;
            for (int i = 0; i < total_border_length; i++){
                if(j==curvature_coords.at(i).comp_id){
                    bc+=1;
                }
            }
            border_count.push_back(bc);
        }



    // CREATES A 2D VECTOR OF CURVATURES WHERE IN i-th COLUMN THE CURVATURES OF (i-1)-th COMPONENT ARE STORED
    vector<vector<int>> curvatures; 
        int counter = 0;
        for (int i=0; i<comp_id_max; i++) { // Shift to left
             vector<int> c1;
                for(int j=0; j< border_count[i]; j++){ // Pass through component's border
                    c1.push_back(curvature_coords.at(counter).curvature); // Because the vector curvature_coords is sorted, it is going to be just collecting  
                    counter+= 1;
                }
                curvatures.push_back(c1); 
        } 


    // STORES NUMBER OF PIXELS IN EACH COMPONENT ON SPECIFIED POSITION
    vector<int> pixels_count;
        for (int i=1; i<=comp_id_max; i++){
            int pc=0;
            for (int x = 0; x < MAX_SIZE_x; ++x) {
                for (int y = 0; y < MAX_SIZE_y; y++) {
                    if(components[x][y] ==i){
                        pc+=1;
                    }
                }
            }
            pixels_count.push_back(pc);
        }


    // CALCULATE RATIO OF THE PERIMETER TO THE AREA FOR EACH COMPONENT
    vector<float> ratio;
        for(int i=0; i<comp_id_max; i++){
            ratio.push_back(((float)border_count.at(i)) / (float)pixels_count.at(i));
        }


    // FINAL OUTPUT - ON i-th ROW IS COMPONENT WITH comp_id=i+1, ON POSITION 0 IS ratio, ON j-th POSITION, j>0 IS NUMBER OF PIXELS WITH curvature=j  
    vector<vector<int>> final_out;
        for (int i=0; i<curvatures.size(); i++){
            vector<int> f1;
                for (int curv=1; curv<=circle_size; curv++){  // Use 80 instead of MAX_CURV_PIX - easier reading
                    int counter_f = 0;
                    for (int j=0; j< curvatures[i].size(); j++){ // Pass through (i+1)-th component (that is placed on i-th position in curvatures)
                        if(curvatures[i][j]==curv){
                        counter_f+=1;
                        }
                    }       
                    f1.push_back(counter_f); // Number of border pixels with curvature (i.e. number of pixels inside cirlce O( ,B_SIZE)) equal to curv is stored on (curv-1)-th position
                }
                final_out.push_back(f1);
        }


    // DISPLAYING A 2D VECTOR
    for (int i = 0; i < final_out.size(); i++) { 
        printf("%f ", ratio[i]);  // On 0-th position puts ratio of primeter and area of component   
        for (int j = 0; j < final_out[i].size(); j++) {
            printf("%d ", final_out[i][j]); // On j-th position places number of border pixels with curvature j
        }       
        printf("\n"); 
    } 


    return 0;
}