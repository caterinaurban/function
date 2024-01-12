/* Peterson's Algorithm from Urban Miné VMCAI 2015 paper
GUARANTEE/RECURRENCE (C1: true)
GUARANTEE/RECURRENCE (C2: true)

suggested parameters:
- partition abstract domain = boxes [default]
- function abstract domain = affine [default]
- backward widening delay = 2 [default]
*/

void main() {
	int flag1 = 0, flag2 = 0, turn;
	
	while (true) {
	// flag1 = 1;
	// turn = 2;
	// await (flag2 == 0 || turn == 1);
	// C1:
	// flag1 = 0;
	//
	// flag2 = 1;
	// turn = 1;
	// await (flag1 == 0 || turn == 2);
	// C2:
	// flag2 = 0;
	if (?) {
		flag1 = 1;
		// turn = 2;
		// await (flag2 == 0 || turn == 1);
		// C1:
		// flag1 = 0;
		//
		// flag2 = 1;
		// turn = 1;
		// await (flag1 == 0 || turn == 2);
		// C2:
		// flag2 = 0;
		if (?) {
			turn = 2;
			// await (flag2 == 0 || turn == 1);
			// C1:
			// flag1 = 0;
			//
			// flag2 = 1;
			// turn = 1;
			// await (flag1 == 0 || turn == 2);
			// C2:
			// flag2 = 0;
			if (flag2 == 0 || turn == 1) {
				// C1:
				// flag1 = 0;
				//
				// flag2 = 1;
				// turn = 1;
				// await (flag1 == 0 || turn == 2);
				// C2:
				// flag2 = 0;
				if (?) {
					C1:
					// flag1 = 0;
					//
					// flag2 = 1;
					// turn = 1;
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (?) {
						flag1 = 0;
						flag2 = 1;
						turn = 1;
						while (flag1 != 0 && turn == 1);
						C2:
						flag2 = 0;
					} else {
						flag2 = 1;
						// flag1 = 0;
						//
						// turn = 1;
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (?) {
							flag1 = 0;
							turn = 1;
							while (flag1 != 0 && turn == 1);
							C2:
							flag2 = 0;
						} else {
							turn = 1;
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					}
				} else {
					flag2 = 1;
					// C1:
					// flag1 = 0;
					//
					// turn = 1;
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (?) {
						C1:
						// flag1 = 0;
						//
						// turn = 1;
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (?) {
							flag1 = 0;
							turn = 1;
							while (flag1 != 0 && turn == 1);
							C2:
							flag2 = 0;
						} else {
							turn = 1;
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						turn = 1;
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					}
				}
			} else {
				flag2 = 1;
				// await (flag2 == 0 || turn == 1);
				// C1:
				// flag1 = 0;
				//
				// turn = 1;
				// await (flag1 == 0 || turn == 2);
				// C2:
				// flag2 = 0;
				if (flag2 == 0 || turn == 1) {
					// C1:
					// flag1 = 0;
					//
					// turn = 1;
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (?) {
						C1:
						// flag1 = 0;
						//
						// turn = 1;
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (?) {
							flag1 = 0;
							turn = 1;
							while (flag1 != 0 && turn == 1);
							C2:
							flag2 = 0;
						} else {
							turn = 1;
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						turn = 1;
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					}
				} else {
					turn = 1;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (flag2 == 0 || turn == 1) {
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					}
				}
			}
		} else {
			flag2 = 1;
			// turn = 2;
			// await (flag2 == 0 || turn == 1);
			// C1:
			// flag1 = 0;
			//
			// turn = 1;
			// await (flag1 == 0 || turn == 2);
			// C2:
			// flag2 = 0;
			if (?) {
				turn = 2;
				// await (flag2 == 0 || turn == 1);
				// C1:
				// flag1 = 0;
				//
				// turn = 1;
				// await (flag1 == 0 || turn == 2);
				// C2:
				// flag2 = 0;
				if (flag2 == 0 || turn == 1) {
					// C1:
					// flag1 = 0;
					//
					// turn = 1;
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (?) {
						C1:
						// flag1 = 0;
						//
						// turn = 1;
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (?) {
							flag1 = 0;
							turn = 1;
							while (flag1 != 0 && turn == 1);
							C2:
							flag2 = 0;
						} else {
							turn = 1;
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						turn = 1;
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					}
				} else {
					turn = 1;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (flag2 == 0 || turn == 1) {
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					}
				}
			} else {
				turn = 1;
				// turn = 2;
				// await (flag2 == 0 || turn == 1);
				// C1:
				// flag1 = 0;
				//
				// await (flag1 == 0 || turn == 2);
				// C2:
				// flag2 = 0;
				if (flag1 == 0 || turn == 2) {
					// turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// C2:
					// flag2 = 0;
					if (?) {
						turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					} else {
						C2:
						// turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// flag2 = 0;
						if (?) {
							turn = 2;
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						} else {
							flag2 = 0;
							turn = 2;
							while (flag2 != 0 && turn == 2);
							C1:
							flag1 = 0;
						}
					}
				} else {
					turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (flag2 == 0 || turn == 1) {
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					}
				}
			}
		}
	} else {
		flag2 = 1;
		// flag1 = 1;
		// turn = 2;
		// await (flag2 == 0 || turn == 1);
		// C1:
		// flag1 = 0;
		//
		// turn = 1;
		// await (flag1 == 0 || turn == 2);
		// C2:
		// flag2 = 0;
		if (?) {
			flag1 = 1;
			// turn = 2;
			// await (flag2 == 0 || turn == 1);
			// C1:
			// flag1 = 0;
			//
			// turn = 1;
			// await (flag1 == 0 || turn == 2);
			// C2:
			// flag2 = 0;
			if (?) {
				turn = 2;
				// await (flag2 == 0 || turn == 1);
				// C1:
				// flag1 = 0;
				//
				// turn = 1;
				// await (flag1 == 0 || turn == 2);
				// C2:
				// flag2 = 0;
				if (flag2 == 0 || turn == 1) {
					// C1:
					// flag1 = 0;
					//
					// turn = 1;
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (?) {
						C1:
						// flag1 = 0;
						//
						// turn = 1;
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (?) {
							flag1 = 0;
							turn = 1;
							while (flag1 != 0 && turn == 1);
							C2:
							flag2 = 0;
						} else {
							turn = 1;
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						turn = 1;
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					}
				} else {
					turn = 1;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (flag2 == 0 || turn == 1) {
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					}
				}
			} else {
				turn = 1;
				// turn = 2;
				// await (flag2 == 0 || turn == 1);
				// C1:
				// flag1 = 0;
				//
				// await (flag1 == 0 || turn == 2);
				// C2:
				// flag2 = 0;
				if (flag1 == 0 || turn == 2) {
					// turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// C2:
					// flag2 = 0;
					if (?) {
						turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					} else {
						C2:
						// turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// flag2 = 0;
						if (?) {
							turn = 2;
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						} else {
							flag2 = 0;
							turn = 2;
							while (flag2 != 0 && turn == 2);
							C1:
							flag1 = 0;
						}
					}
				} else {
					turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (flag2 == 0 || turn == 1) {
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					}
				}
			}
		} else {
			turn = 1;
			// flag1 = 1;
			// turn = 2;
			// await (flag2 == 0 || turn == 1);
			// C1:
			// flag1 = 0;
			//
			// await (flag1 == 0 || turn == 2);
			// C2:
			// flag2 = 0;
			if (flag1 == 0 || turn == 2) {
				// flag1 = 1;
				// turn = 2;
				// await (flag2 == 0 || turn == 1);
				// C1:
				// flag1 = 0;
				//
				// C2:
				// flag2 = 0;
				if (?) {
					flag1 = 1;
					// turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// C2:
					// flag2 = 0;
					if (?) {
						turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					} else {
						C2:
						// turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// flag2 = 0;
						if (?) {
							turn = 2;
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						} else {
							flag2 = 0;
							turn = 2;
							while (flag2 != 0 && turn == 2);
							C1:
							flag1 = 0;
						}
					}
				} else {
					C2:
					// flag1 = 1;
					// turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// flag2 = 0;
					if (?) {
						flag1 = 1;
						// turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// flag2 = 0;
						if (?) {
							turn = 2;
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						} else {
							flag2 = 0;
							turn = 2;
							while (flag2 != 0 && turn == 2);
							C1:
							flag1 = 0;
						}
					} else {
						flag2 = 0;
						flag1 = 1;
						turn = 2;
						while (flag2 != 0 && turn == 2);
						C1:
						flag1 = 0;
					}
				}
			} else {
				flag1 = 1;
				// turn = 2;
				// await (flag2 == 0 || turn == 1);
				// C1:
				// flag1 = 0;
				//
				// await (flag1 == 0 || turn == 2);
				// C2:
				// flag2 = 0;
				if (flag1 == 0 || turn == 2) {
					// turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// C2:
					// flag2 = 0;
					if (?) {
						turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					} else {
						C2:
						// turn = 2;
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// flag2 = 0;
						if (?) {
							turn = 2;
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						} else {
							flag2 = 0;
							turn = 2;
							while (flag2 != 0 && turn == 2);
							C1:
							flag1 = 0;
						}
					}
				} else {
					turn = 2;
					// await (flag2 == 0 || turn == 1);
					// C1:
					// flag1 = 0;
					//
					// await (flag1 == 0 || turn == 2);
					// C2:
					// flag2 = 0;
					if (flag2 == 0 || turn == 1) {
						// C1:
						// flag1 = 0;
						//
						// await (flag1 == 0 || turn == 2);
						// C2:
						// flag2 = 0;
						if (flag1 == 0 || turn == 2) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C1:
							// flag1 = 0;
							//
							// await (flag1 == 0 || turn == 2);
							// C2:
							// flag2 = 0;
							if (flag1 == 0 || turn == 2) {
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								flag1 = 0;
								while (flag1 != 0 && turn == 1);
								C2:
								flag2 = 0;
							}
						}
					} else {
						// await (flag2 == 0 || turn == 1);
						// C1:
						// flag1 = 0;
						//
						// C2:
						// flag2 = 0;
						if (flag2 == 0 || turn == 1) {
							// C1:
							// flag1 = 0;
							//
							// C2:
							// flag2 = 0;
							if (?) {
								C1:
								// flag1 = 0;
								//
								// C2:
								// flag2 = 0;
								if (?) {
									flag1 = 0;
									C2:
									flag2 = 0;
								} else {
									C2:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								}
							} else {
								C2:
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							}
						} else {
							C2:
							// await (flag2 == 0 || turn == 1);
							// C1:
							// flag1 = 0;
							//
							// flag2 = 0;
							if (flag2 == 0 || turn == 1) {
								// C1:
								// flag1 = 0;
								//
								// flag2 = 0;
								if (?) {
									C1:
									// flag1 = 0;
									//
									// flag2 = 0;
									if (?) {
										flag1 = 0;
										flag2 = 0;
									} else {
										flag2 = 0;
										flag1 = 0;
									}
								} else {
									flag2 = 0;
									C1:
									flag1 = 0;
								}
							} else {
								flag2 = 0;
								while (flag2 != 0 && turn == 2);
								C1:
								flag1 = 0;
							}
						}
					}
				}
			}
		}
	}
	}

}
