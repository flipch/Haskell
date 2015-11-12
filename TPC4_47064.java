package trabalho4;

//import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.function.BiFunction;

/**
 * @author fc47064
 *
 */
public class TPC4_47064 {
    
	/* TESTES TESTES TESTES DONT MIND ME 
	public static void main(String[] args) {

		List<Integer> numeros = Arrays.asList(1, 2, 3, 4, 5, 6, 7);
		System.out.println(foldl(((acc, e) -> acc + e * e), 0, numeros));

		List<String> lista = Arrays.asList("Ana,30,RH,1500,3",
				"Rui,40,Vendas,1000,2", "Luis,45,RH,3333,5",
				"Maria,55,Admin,2000,4");
		System.out.println(maximo(Comparator.comparing(s -> s.split(",")[1]),
				lista));

		List<Funcionario> firma = Arrays.asList(new Funcionario("Ana", 30,
				"RH", 1500, 3), new Funcionario("Rui", 40, "Vendas", 1000, 2),
				new Funcionario("Luis", 45, "RH", 3333, 5), new Funcionario(
						"Maria", 55, "Admin", 2004, 4));

		System.out.println(maiorOrdenado(firma));
	}
	*/
	
	/**
	 * Aplica uma funcao que recebe dois elementos sendo estes um acumulador e
	 * cada elemento de uma lista a começar da esquerda.
	 * 
	 * @param funcao
	 *             Função a ser aplicada
	 * @param zero
	 *             Acumulador começado em zero
	 * @param lista
	 *             Lista onde aplicar a função com o acumulador
	 * @return zero  Valor acumulado
	 */
	public static <T, R> R foldl(BiFunction<R, T, R> funcao, R zero,
			List<T> lista) {
		for (int index = 0; index < lista.size(); index++)
			zero = funcao.apply(zero, lista.get(index));

		return zero;
	}

	/**
	 * Devolve o elemento máximo da lista
	 * 
	 * @param comparador Comparador a ser utilizado
	 * @param lista Lista de elementos
	 * @return maximo - Elemento máximo da lista
	 */
	public static <T> T maximo(Comparator<T> comparador, List<T> lista) {
		T maximo = null;
		for (int index = 0; index < lista.size(); index++) {
			if (index == 0)
				maximo = lista.get(0);
			else {
				if (comparador.compare(lista.get(index), maximo) > 0)
					maximo = lista.get(index);
			}

		}
		return maximo;
	}

	/**
	 * Devolve o Funcionário com o maior ordenado numa lista
	 * 
	 * @param lista Lista de elementos
	 * @return Funcionario - Funcionario com maior ordenado
	 */
	public static Funcionario maiorOrdenado(List<Funcionario> lista) {
		Funcionario chefe = null;

		for (int index = 0; index < lista.size(); index++) {
			if (index == 0)
				chefe = lista.get(0);
			else {
				if (lista.get(index).ordenado > chefe.ordenado)
					chefe = lista.get(index);
			}

		}

		return chefe;
	}

}

/*
 * Escreva uma simples classe Funcionario que represente um funcionário numa
 * firma. A classe deverá conter um atributo para cada informação relativa a um
 * funcionário: o seu nome, idade, departamento onde trabalha, ordenado e
 * dimensão do agregado familiar. Junte um método toString() adequado.
 */

/**
 * @author fc47064
 *
 */
class Funcionario {

	String nome, departamento;
	int idade, ordenado, dimensao_familia;

	/**
	 * Construtor da classe Funcionario
	 * 
	 * @param n
	 *             nome
	 * @param i
	 *             idade
	 * @param d
	 *             departamento
	 * @param o
	 *             ordenado
	 * @param s
	 *             dimensao da familia
	 */
public Funcionario(String n, int i, String d, int o, int s) {
		this.nome = n;
		this.idade = i;
		this.departamento = d;
		this.ordenado = o;
		this.dimensao_familia = s;
	}

	@Override
	public String toString() {
		return "Funcionario [nome=" + nome + ", idade=" + idade
				+ ", departamento=" + departamento + ", ordenado=" + ordenado
				+ ", dimensao_familia=" + dimensao_familia + "]";
	}

}
