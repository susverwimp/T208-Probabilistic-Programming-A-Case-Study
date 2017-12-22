import java.util.Arrays;

public class Test {
	
	int width;
	int height;
	String[][] blocks;
	int sampleIndex = 1;
	int maxSampleIndex;
	
	Test(int width, int height, int maxSampleIndex) {
		this.width = width;
		this.height = height;
		this.maxSampleIndex = maxSampleIndex;
		blocks = new String[height][width];
		choose(height, 0, createInitial(width, height));
	}
	
	public String[][][] createInitial(int width, int height){
		String[] initial = {"red","green","blue","yellow"};
		String[][][] result = new String[width][height][4];
		for (int i = 0; i < width; i++) {
			for (int j = 0; j < height; j++) {
				System.arraycopy(initial, 0, result[i][j], 0, initial.length);
			}
		}
		return result;
	}
	
	public static String[][][] copy_initial(String[][][] initial){
		String[][][] copy = new String[initial.length][initial[0].length][initial[0][0].length];
		for (int i = 0; i < initial.length; i++) {
			for (int j = 0; j < initial[0].length; j++) {
				for (int k = 0; k < initial[0][0].length; k++) {
					copy[i][j][k] = initial[i][j][k];
				}
			}
		}
		return copy;
	}
	
	public void choose(int i, int j, String[][][] initial) {
		String[][][] copy = copy_initial(initial);
		for (String color : copy[i][j]) {
			if(color == null)
				continue;
			blocks[i][j] = color;
			if(j > 0){
				if(j < width - 1 && color.equals(blocks[i][j-1])){
					switch(color) {
					case "red":
						copy[i][j+1][0] = null;
						break;
					case "green":
						copy[i][j+1][1] = null;
						break;
					case "blue":
						copy[i][j+1][2] = null;
						break;
					case "yellow":
						copy[i][j+1][3] = null;
						break;
					}
					
				}
			}
			if(i < height - 1){
				if(i > 0 && color.equals(blocks[i-1][j])){
					switch(color) {
					case "red":
						copy[i-1][j][0] = null;
						break;
					case "green":
						copy[i-1][j][1] = null;
						break;
					case "blue":
						copy[i-1][j][2] = null;
						break;
					case "yellow":
						copy[i-1][j][3] = null;
						break;
					}
				}
			}
			if(j < width - 1)
				choose(i,j+1,copy);
			else if(j == width - 1)
				if(i > 0)
					choose(i-1, 0,copy);
				else if(i == 0) {
					System.out.println(Arrays.deepToString(blocks));
					if(maxSampleIndex > 0 && ++sampleIndex == maxSampleIndex + 1)
						System.exit(0);
				}
		}
	}
	
	public static void main(String[] args) {
		int width = 3;
		int height = 3;
		int maxSampleIndex = -1;
		if(args.length == 1) {
			width = Integer.parseInt(args[0]);
			height = Integer.parseInt(args[0]);
		} else if(args.length == 2) {
			width = Integer.parseInt(args[0]);
			height = Integer.parseInt(args[1]);
		} else if(args.length == 3) {
			width = Integer.parseInt(args[0]);
			height = Integer.parseInt(args[1]);
			maxSampleIndex = Integer.parseInt(args[2]);
		}
		new Test(width,height,maxSampleIndex);
	}

}